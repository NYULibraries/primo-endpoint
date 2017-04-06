{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Config
  ( Collection(..)
  , Collections
  , Config(..)
  , loadConfig
  , loadSource
  ) where

import           Control.Arrow ((***))
import           Control.Monad (guard, when, liftM2)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8)
import           Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import qualified Data.Vector as V
import qualified Data.Yaml as YAML
import           Network.URI (URI, parseAbsoluteURI)
import           System.FilePath ((</>), (<.>))
import           Text.Read (readMaybe)

import           Util
import           Document
import           Fields
import           ISO639
import           Source.JSON
import           Source.FDA
import           Source.DLTS
import           Source.DLib
import           Source.SDR
import           Source.SpecialCollections

type Interval = NominalDiffTime

-- |Bootstrapping configuration values, needed to load the rest of the config file.
data PreConfig = PreConfig
  { configInterval :: Interval
  , configFDACollections :: Int
  }

instance JSON.FromJSON PreConfig where
  parseJSON = JSON.withObject "pre-config" $ \o -> PreConfig
    <$> o JSON..:? "interval" JSON..!= 0
    <*> (o JSON..:? "fda" JSON..!= HMap.empty >>= (JSON..!= 0) . (JSON..:? "collections"))

-- |Cached indices for converting to collection identifiers.
-- Currenly only used for FDA.
data Indices = Indices
  { fdaIndex :: HMap.HashMap Int Int -- ^map from FDA handle suffix to database id
  } deriving (Show, Read)

loadIndices :: PreConfig -> IO Indices
loadIndices conf = Indices
  <$> loadFDAIndex (configFDACollections conf)

-- |Possible metadata sources.
-- These correspond to modules in "Source" and the collection source config key.
data Source
  = SourceJSON URI
  | SourceFDA Int
  | SourceDLTS DLTSCore T.Text
  | SourceDLib BS.ByteString
  | SourceSDR
  | SourceSpecialCollections [(BS.ByteString, BS.ByteString)]

data Collection = Collection
  { collectionKey :: !T.Text -- ^Unique key for this collection
  , collectionSource :: !Source
  , collectionCache :: FilePath -- ^JSON cache file for processed 'Document's
  , collectionInterval :: !Interval -- ^Max cache file age before reloading
  , collectionName :: Maybe T.Text
  , collectionFields :: Generators -- ^Metadata field mapping
  }

-- |Map from 'collectionKey' to 'Collection'
type Collections = HMap.HashMap T.Text Collection

-- |A loaded configuration
data Config = Config
  { configCollections :: Collections
  , configVerbose :: !Bool
  }

-- |Values used during loading the config file
data Env = Env
  { envPreConfig :: !PreConfig
  , envCache :: !FilePath -- ^Cache directory
  , envISO639 :: !ISO639
  , envIndices :: !Indices
  , envGenerators :: !Generators -- ^Generator macros expanded during loading 'collectionFields'
  , envTemplates :: !(HMap.HashMap T.Text Generators) -- ^Templates that can be included in 'collectionFields'
  , envVerbose :: !Bool
  }

fixLanguage :: ISO639 -> Generators -> Generators
fixLanguage iso = HMap.adjust (languageGenerator iso) "language"

-- |@parseSource collection source_type@
parseSource :: Env -> JSON.Object -> T.Text -> JSON.Parser Source
parseSource _ o "JSON" = SourceJSON
  <$> (maybe (maybe (fail "Invalid URI") return . parseAbsoluteURI =<< o JSON..: "url")
    (return . fileURI) =<< o JSON..:? "file")
parseSource env o "FDA" = SourceFDA
  <$> (maybe (o JSON..: "id")
    (\h -> maybe (fail "Unknown FDA handle") return $ HMap.lookup h (fdaIndex $ envIndices env)) =<< o JSON..:? "hdl")
parseSource _ o "DLTS" = SourceDLTS
  <$> o JSON..: "core"
  <*> o JSON..: "code"
parseSource _ o "DLib" = SourceDLib
  <$> (TE.encodeUtf8 <$> o JSON..: "path")
parseSource _ _ "SDR" = return SourceSDR
parseSource _ o "SpecialCollections" = SourceSpecialCollections
  <$> (map (TE.encodeUtf8 *** TE.encodeUtf8) . HMap.toList <$> o JSON..: "filters")
parseSource _ _ s = fail $ "Unknown collection source: " ++ show s

-- |@parseCollection generators templates key value@
parseCollection :: Env -> T.Text -> JSON.Value -> JSON.Parser Collection
parseCollection env key = JSON.withObject "collection" $ \o -> do
  s <- parseSource env o =<< o JSON..: "source"
  i <- o JSON..:? "interval"
  n <- o JSON..:? "name"
  f <- parseGenerators (envGenerators env) =<< o JSON..:? "fields" JSON..!= JSON.Null
  t <- withArrayOrNullOrSingleton (foldMapM getTemplate) =<< o JSON..:? "template" JSON..!= JSON.Null
  return Collection
    { collectionKey = key
    , collectionCache = envCache env </> T.unpack key <.> "json"
    , collectionSource = s
    , collectionInterval = fromMaybe (configInterval $ envPreConfig env) i
    , collectionName = n
    , collectionFields = fixLanguage (envISO639 env) $ f <> t
    }
  where
  getTemplate = JSON.withText "template name" $ \s ->
    maybe (fail $ "Undefined template: " ++ show s) return $ HMap.lookup s $ envTemplates env

parseConfig :: Env -> JSON.Value -> JSON.Parser Config
parseConfig env = JSON.withObject "config" $ \o -> do
  g <- o JSON..:? "generators" JSON..!= mempty
  t <- withObjectOrNull "templates" (mapM $ parseGenerators g) =<< o JSON..:? "templates" JSON..!= JSON.Null
  c <- JSON.withObject "collections" (HMap.traverseWithKey $ parseCollection env{ envGenerators = g <> envGenerators env, envTemplates = t <> envTemplates env })
    =<< o JSON..: "collections"
  return Config
    { configCollections = c
    , configVerbose = envVerbose env
    }

updateIndices :: Bool -> PreConfig -> FilePath -> IO Indices
updateIndices force pc f = maybe (do
    idx <- loadIndices pc
    writeFile f $ show idx
    return idx)
  return =<< runMaybeT (do
    guard $ not force
    d <- MaybeT $ Just <$> liftM2 diffUTCTime getCurrentTime (getModificationTime0 f)
    guard $ d < configInterval pc
    MaybeT $ readMaybe <$> readFile f)

-- @loadConfig force cacheDir confFile@
loadConfig :: Bool -> FilePath -> FilePath -> Bool -> IO Config
loadConfig force cache conf verb = do
  jc <- fromMaybe JSON.Null <$> YAML.decodeFile conf
  pc <- parseJSONM jc
  idx <- updateIndices force pc (cache </> "index")
  iso <- loadISO639 (cache </> "iso639")
  parseM (parseConfig Env
    { envPreConfig = pc
    , envCache = cache
    , envISO639 = iso
    , envIndices = idx
    , envGenerators = HMap.empty
    , envTemplates = HMap.empty
    , envVerbose = verb
    }) jc

loadSource :: Config -> Collection -> IO Documents
loadSource Config{ configVerbose = verb } c = do
  when verb $ putStrLn $ "Loading " ++ cs
  r <- ls $ collectionSource c
  when (V.null r) $ fail $ cs ++ ": no documents returned"
  return r
  where
  ls (SourceJSON u) = loadJSON u
  ls (SourceFDA i) = loadFDA i
  ls (SourceDLTS s i) = loadDLTS s i fl
  ls (SourceDLib p) = loadDLib p
  ls SourceSDR = loadSDR
  ls (SourceSpecialCollections f) = loadSpecialCollections f
  fl = generatorsFields $ collectionFields c
  cs = T.unpack $ collectionKey c
