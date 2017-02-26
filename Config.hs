{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Config
  ( Collection(..)
  , Collections
  , Config(..)
  , loadConfig
  , loadCollection
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime)
import qualified Data.Vector as V
import qualified Data.Yaml as YAML
import           System.FilePath ((</>), (<.>))

import           Util
import           Document
import           Fields
import           FDA

type Interval = NominalDiffTime

data PreConfig = PreConfig
  { configFDACollections :: Int
  }

instance JSON.FromJSON PreConfig where
  parseJSON = JSON.withObject "pre-config" $ \o -> PreConfig
    <$> (o JSON..: "fda" >>= (JSON..: "collections"))

data Indices = Indices
  { fdaIndex :: HM.HashMap Int Int
  }

loadIndices :: PreConfig -> IO Indices
loadIndices conf = Indices
  <$> loadFDAIndex (configFDACollections conf)

data Source
  = SourceFDA
    { _fdaCollectionId :: Int
    }
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
parseCollection :: Indices -> FilePath -> Interval -> Generators -> HM.HashMap T.Text Generators -> T.Text -> JSON.Value -> JSON.Parser Collection
parseCollection idx cache int gen tpl key = JSON.withObject "collection" $ \o -> do
  s <- parseSource idx o =<< o JSON..: "source"
  i <- o JSON..:? "interval"
  n <- o JSON..:? "name"
  f <- parseGenerators gen =<< o JSON..:? "fields" JSON..!= JSON.Null
  t <- withArrayOrNullOrSingleton (foldMapM getTemplate) =<< o JSON..:? "template" JSON..!= JSON.Null
  return Collection
    { collectionCache = cache </> T.unpack key <.> "json"
    , collectionSource = s
    , collectionInterval = fromMaybe int i
    , collectionName = n
    , collectionFields = f <> t
    }
  where
  getTemplate = JSON.withText "template name" $ \s ->
    maybe (fail $ "Undefined template: " ++ show s) return $ HM.lookup s tpl

parseConfig :: Indices -> FilePath -> JSON.Value -> JSON.Parser Config
parseConfig idx cache = JSON.withObject "config" $ \o -> do
  i <- o JSON..: "interval"
  g <- o JSON..:? "generators" JSON..!= mempty
  t <- withObjectOrNull "templates" (mapM $ parseGenerators g) =<< o JSON..:? "templates" JSON..!= JSON.Null
  c <- JSON.withObject "collections" (HM.traverseWithKey $ parseCollection idx cache i g t)
    =<< o JSON..: "collections"
  return Config
    { configCollections = c
    , configCache = cache </> "json"
    }

loadConfig :: FilePath -> FilePath -> IO Config
loadConfig cache conf = do
  Just jc <- YAML.decodeFile conf
  pc <- parseJSONM jc
  idx <- loadIndices pc
  parseM (parseConfig idx cache) jc

loadSource :: Source -> IO Documents
loadSource (SourceFDA i) = loadFDA i

loadCollection :: Collection -> IO Documents
loadCollection Collection{..} =
  V.map (mapMetadata $ generateFields collectionFields) <$> loadSource collectionSource
