{-# LANGUAGE OverloadedStrings #-}
module Config
  ( Source(..)
  , Collection(..)
  , Config(..)
  , loadSource
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T

import           Util
import           Document
import           Fields
import           FDA

data Source
  = SourceFDA
    { fdaCollectionId :: Int
    }

data Collection = Collection
  { collectionSource :: Source
  , collectionName :: Maybe T.Text
  , collectionFields :: Generators
  }

data Config = Config
  { configCollections :: HM.HashMap T.Text Collection
  , configInterval :: Int
  }

-- |@parseSource collection source_type@
parseSource :: JSON.Object -> T.Text -> JSON.Parser Source
parseSource o "FDA" = SourceFDA <$> o JSON..: "id"
parseSource _ s = fail $ "Unknown collection source: " ++ show s

-- |@parseCollection generators templates key value@
parseCollection :: Generators -> HM.HashMap T.Text Generators -> JSON.Value -> JSON.Parser Collection
parseCollection gen tpl = JSON.withObject "collection" $ \o -> do
  s <- parseSource o =<< o JSON..: "source"
  n <- o JSON..:? "name"
  f <- parseGenerators gen =<< o JSON..:? "fields" JSON..!= JSON.Null
  t <- withArrayOrNullOrSingleton (foldMapM getTemplate) =<< o JSON..:? "template" JSON..!= JSON.Null
  return Collection
    { collectionSource = s
    , collectionName = n
    , collectionFields = f <> t
    }
  where
  getTemplate = JSON.withText "template name" $ \s ->
    maybe (fail $ "Undefined template: " ++ show s) return $ HM.lookup s tpl

instance JSON.FromJSON Config where
  parseJSON = JSON.withObject "config" $ \o -> do
    i <- o JSON..: "interval"
    g <- o JSON..:? "generators" JSON..!= mempty
    t <- withObjectOrNull "templates" (mapM $ parseGenerators g) =<< o JSON..:? "templates" JSON..!= JSON.Null
    c <- JSON.withObject "collections" (mapM $ parseCollection g t) =<< o JSON..: "collections"
    return Config
      { configCollections = c
      , configInterval = i
      }

loadSource :: Source -> IO [Document]
loadSource (SourceFDA i) = loadFDA i
