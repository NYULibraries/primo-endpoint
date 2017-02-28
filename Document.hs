{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Document
  ( Value(..)
  , valueOr
  , Metadata
  , addMetadata
  , Document(..)
  , mapMetadata
  , Documents
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (ParseTime(..), formatTime)
import qualified Data.Vector as V

newtype Value = Value{ values :: [T.Text] }
  deriving (Eq, Ord, Monoid, Show)

instance JSON.FromJSON Value where
  parseJSON (JSON.String t) = return $ Value [t]
  parseJSON (JSON.Number a) = return $ Value [T.pack $ show a]
  parseJSON (JSON.Bool True) = return $ Value ["true"]
  parseJSON (JSON.Bool False) = return $ Value ["false"]
  parseJSON (JSON.Array a) = Value . concatMap values <$> mapM JSON.parseJSON a
  parseJSON v = JSON.typeMismatch "Value" v

instance JSON.ToJSON Value where
  toJSON (Value l) = JSON.Array $ V.fromList $ map JSON.String l

instance ParseTime Value where
#if MIN_VERSION_time(1,6,0)
  buildTime _ [] = Just $ Value []
  buildTime l x = (Value . return . T.pack . formatTime l fmt) <$> (buildTime l x :: Maybe UTCTime) where
#else
  buildTime _ [] = Value []
  buildTime l x = Value $ return $ T.pack $ formatTime l fmt (buildTime l x :: UTCTime) where
#endif
    fmt = chk "CfYGygs" ("%Y"
      ++  chk "BbmVUWjs" ("-%m"
      ++  chk "deuaAwjs" ("-%d"
      ++  chk "HkIls" ("T%H"
      ++  chk "Ms" (":%M"
      ++  chk "Ss" ":%S%Q")
      ++  chk "Zz" "Z"))))
    c = map fst x
    chk s f
      | any (`elem` c) (s :: String) = f
      | otherwise = ""

maybeValue :: Value -> Maybe Value
maybeValue (Value []) = Nothing
maybeValue v = Just v

valueJSON :: Value -> Maybe JSON.Value
valueJSON v = JSON.toJSON <$> maybeValue v

valueOr :: Value -> Value -> Value
valueOr a b = fromMaybe b $ maybeValue a

type Metadata = HM.HashMap T.Text Value

addMetadata :: Metadata -> T.Text -> T.Text -> Metadata
addMetadata m k v = HM.insertWith (flip mappend) k (Value [v]) m

data Document = Document
  { documentID :: T.Text
  , documentCollection :: T.Text
  , documentModified :: UTCTime
  , documentMetadata :: Metadata
  } deriving (Show)

instance JSON.ToJSON Document where
  toJSON Document{..} = JSON.Object
    $ HM.insert "id" (JSON.String documentID)
    $ HM.insert "collection_ssm" (JSON.String documentCollection)
    $ HM.fromList $ mapMaybe (\(k, v) -> (,) ("desc_metadata__" <> k <> "_tesim") <$> valueJSON v) $ HM.toList documentMetadata

mapMetadata :: (Metadata -> Metadata) -> Document -> Document
mapMetadata f d = d{ documentMetadata = f $ documentMetadata d }

type Documents = V.Vector Document
