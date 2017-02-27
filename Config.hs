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
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import qualified Data.Vector as V
import qualified Data.Yaml as YAML
import           System.FilePath ((</>), (<.>))
import           Text.Read (readMaybe)

import           Util
import           Document
import           Fields
import           FDA

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
  { fdaIndex :: HM.HashMap Int Int
  } deriving (Show, Read)

loadIndices :: PreConfig -> IO Indices
loadIndices conf = Indices
  <$> loadFDAIndex (configFDACollections conf)

data Source
  = SourceFDA Int
  deriving (Show)

data Collection = Collection
  { collectionSource :: Source
  , collectionCache :: FilePath
  , collectionInterval :: Interval
  , collectionName :: Maybe T.Text
  , collectionFields :: Generators
  }

type Collections = HM.HashMap T.Text Collection

data Config = Config
  { configCollections :: Collections
  , configCache :: FilePath
  }

-- |@parseSource collection source_type@
parseSource :: Indices -> JSON.Object -> T.Text -> JSON.Parser Source
parseSource idx o "FDA" = SourceFDA <$>
  (maybe (o JSON..: "id") (\h -> maybe (fail "Unknown FDA handle") return $ HM.lookup h (fdaIndex idx)) =<< o JSON..:? "hdl")
parseSource _ _ s = fail $ "Unknown collection source: " ++ show s

-- |@parseCollection generators templates key value@
parseCollection :: PreConfig -> Indices -> FilePath -> Generators -> HM.HashMap T.Text Generators -> T.Text -> JSON.Value -> JSON.Parser Collection
parseCollection pc idx cache gen tpl key = JSON.withObject "collection" $ \o -> do
  s <- parseSource idx o =<< o JSON..: "source"
  i <- o JSON..:? "interval"
  n <- o JSON..:? "name"
  f <- parseGenerators gen =<< o JSON..:? "fields" JSON..!= JSON.Null
  t <- withArrayOrNullOrSingleton (foldMapM getTemplate) =<< o JSON..:? "template" JSON..!= JSON.Null
  return Collection
    { collectionCache = cache </> T.unpack key <.> "json"
    , collectionSource = s
    , collectionInterval = fromMaybe (configInterval pc) i
    , collectionName = n
    , collectionFields = f <> t
    }
  where
  getTemplate = JSON.withText "template name" $ \s ->
    maybe (fail $ "Undefined template: " ++ show s) return $ HM.lookup s tpl

parseConfig :: PreConfig -> Indices -> FilePath -> JSON.Value -> JSON.Parser Config
parseConfig pc idx cache = JSON.withObject "config" $ \o -> do
  g <- o JSON..:? "generators" JSON..!= mempty
  t <- withObjectOrNull "templates" (mapM $ parseGenerators g) =<< o JSON..:? "templates" JSON..!= JSON.Null
  c <- JSON.withObject "collections" (HM.traverseWithKey $ parseCollection pc idx cache g t)
    =<< o JSON..: "collections"
  return Config
    { configCollections = c
    , configCache = cache </> "json"
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
  Just jc <- YAML.decodeFile conf
  pc <- parseJSONM jc
  idx <- updateIndices force pc (cache </> "index")
  parseM (parseConfig pc idx cache) jc

loadSource :: Source -> IO Documents
loadSource (SourceFDA i) = loadFDA i

loadCollection :: Collection -> IO Documents
loadCollection Collection{..} =
  V.map (mapMetadata $ generateFields collectionFields) <$> loadSource collectionSource
