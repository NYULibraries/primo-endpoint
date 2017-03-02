{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Config
  ( Collection(..)
  , Collections
  , Config(..)
  , loadConfig
  , loadCollection
  ) where

import           Control.Monad (guard, liftM2)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import           Data.Foldable (fold)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8)
import           Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import qualified Data.Vector as V
import qualified Data.Yaml as YAML
import           System.FilePath ((</>), (<.>))
import           Text.Read (readMaybe)

import           Util
import           Document
import           Fields
import           ISO639
import           Source.FDA
import           Source.DLTS
import           Source.DLib

type Interval = NominalDiffTime

data PreConfig = PreConfig
  { configInterval :: Interval
  , configFDACollections :: Int
  }

instance JSON.FromJSON PreConfig where
  parseJSON = JSON.withObject "pre-config" $ \o -> PreConfig
    <$> o JSON..: "interval"
    <*> (o JSON..: "fda" >>= (JSON..: "collections"))

data Indices = Indices
  { fdaIndex :: HMap.HashMap Int Int
  } deriving (Show, Read)

loadIndices :: PreConfig -> IO Indices
loadIndices conf = Indices
  <$> loadFDAIndex (configFDACollections conf)

data Source
  = SourceFDA Int
  | SourceDLTS DLTSCore T.Text
  | SourceDLib BS.ByteString
  deriving (Show)

data Collection = Collection
  { collectionKey :: T.Text
  , collectionSource :: Source
  , collectionCache :: FilePath
  , collectionInterval :: Interval
  , collectionName :: Maybe T.Text
  , collectionFields :: Generators
  }

type Collections = HMap.HashMap T.Text Collection

data Config = Config
  { configCollections :: Collections
  , configCache :: FilePath
  }

data Env = Env
  { envPreConfig :: !PreConfig
  , envCache :: !FilePath
  , envISO639 :: !ISO639
  , envIndices :: !Indices
  , envGenerators :: !Generators
  , envTemplates :: !(HMap.HashMap T.Text Generators)
  }

fixLanguage :: ISO639 -> Generators -> Generators
fixLanguage iso = HMap.adjust (languageGenerator iso) "language"

-- |@parseSource collection source_type@
parseSource :: Env -> JSON.Object -> T.Text -> JSON.Parser Source
parseSource env o "FDA" = SourceFDA <$>
  (maybe (o JSON..: "id") (\h -> maybe (fail "Unknown FDA handle") return $ HMap.lookup h (fdaIndex $ envIndices env)) =<< o JSON..:? "hdl")
parseSource _ o "DLTS" = SourceDLTS <$> o JSON..: "core" <*> o JSON..: "code"
parseSource _ o "DLib" = SourceDLib . TE.encodeUtf8 <$> o JSON..: "path"
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
    , configCache = envCache env </> "json"
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

loadConfig :: Bool -> FilePath -> FilePath -> IO Config
loadConfig force cache conf = do
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
    }) jc

loadCollection :: Collection -> IO Documents
loadCollection Collection{..} =
  V.map (mapMetadata $ generateFields collectionFields) <$> loadSource collectionSource where
  loadSource (SourceFDA i) = loadFDA i
  loadSource (SourceDLTS c i) = loadDLTS collectionKey collectionName c i fl
  loadSource (SourceDLib p) = loadDLib collectionKey (fold collectionName) p
  fl = generatorsFields collectionFields
