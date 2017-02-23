{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Document
  ( Value(..)
  , Metadata
  , addMetadata
  , Document(..)
  ) where

import           Control.Arrow ((***))
import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Lazy as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V

newtype Value = Value{ values :: [T.Text] }
  deriving (Eq, Ord, Monoid, Show)

instance JSON.FromJSON Value where
  parseJSON (JSON.String t) = return $ Value [t]
  parseJSON (JSON.Array a) = Value . concat <$> mapM JSON.parseJSON a
  parseJSON v = JSON.typeMismatch "Value" v

instance JSON.ToJSON Value where
  toJSON (Value l) = JSON.Array $ V.fromList $ map JSON.String l

type Metadata = HM.HashMap T.Text Value

addMetadata :: Metadata -> T.Text -> T.Text -> Metadata
addMetadata m k v = HM.insertWith (flip mappend) k (Value [v]) m

data Document = Document
  { documentID :: T.Text
  , documentCollection :: T.Text
  , documentMetadata :: Metadata
  }

instance JSON.ToJSON Document where
  toJSON (Document i c m) = JSON.Object
    $ HM.insert "id" (JSON.String i)
    $ HM.insert "collection_ssm" (JSON.String c)
    $ HM.insert "desc_metadata__type_tesim" (JSON.String "Report")
    $ HM.fromList $ map (("desc_metadata__" <>) . (<> "_tesim") *** JSON.toJSON) $ HM.toList m
