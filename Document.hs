{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Document
  ( Value(..)
  , mapValues
  , value
  , oneValue
  , parseValue
  , valueOr
  , Metadata
  , getMetadata
  , addMetadata
  , Document(..)
  , mapMetadata
  , Documents
  , handleToID
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as HMap
import           Data.Foldable (fold, foldlM)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (ParseTime(..), formatTime)
import qualified Data.Vector as V

-- |Metadata values can always be multiple
newtype Value = Value{ values :: [T.Text] }
  deriving (Eq, Ord, Monoid, Show)

mapValues :: (T.Text -> T.Text) -> Value -> Value
mapValues f = Value . map f . values

-- |Single value
value :: T.Text -> Value
value = Value . return

-- |@'oneValue' . 'value' == return@, othewise 'fail'.
oneValue :: Monad m => Value -> m T.Text
oneValue (Value [v]) = return v
oneValue v = fail $ "expecting single value: " ++ show v

-- |Values can be parsed from any JSON type except objects
instance JSON.FromJSON Value where
  parseJSON (JSON.String t) = return $ value t
  parseJSON (JSON.Number a) = return $ value $ T.pack $ show a
  parseJSON (JSON.Bool True) = return $ value "true"
  parseJSON (JSON.Bool False) = return $ value "false"
  parseJSON (JSON.Array a) = Value . concatMap values <$> mapM JSON.parseJSON a
  parseJSON v = JSON.typeMismatch "Value" v

-- |A more permissive (total) parser for Values that produces empty for unparsable values (i.e., objects)
parseValue :: JSON.Value -> Value
parseValue = fold . JSON.parseMaybe JSON.parseJSON

-- |When outputing values, they're always an array of strings
instance JSON.ToJSON Value where
  toJSON (Value l) = JSON.Array $ V.fromList $ map JSON.String l

-- |Value has a canonical time representation that we can parse from strings:
-- the prefix of @%Y-%m-%dT%H:%M:%S%QZ@ only including fields with parsed values.
instance ParseTime Value where
#if MIN_VERSION_time(1,6,0)
  buildTime _ [] = Just $ Value []
  buildTime l x = (value . T.pack . formatTime l fmt) <$> (buildTime l x :: Maybe UTCTime) where
#else
  buildTime _ [] = Value []
  buildTime l x = value $ T.pack $ formatTime l fmt (buildTime l x :: UTCTime) where
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

-- |Nothing for empty values
maybeValue :: Value -> Maybe Value
maybeValue (Value []) = Nothing
maybeValue v = Just v

-- |JSON for non-empty values
valueJSON :: Value -> Maybe JSON.Value
valueJSON = fmap JSON.toJSON . maybeValue

-- |The alternative monoid append, left-biased
valueOr :: Value -> Value -> Value
valueOr (Value []) a = a
valueOr a _ = a

type Metadata = HMap.HashMap T.Text Value

-- |Lookup a single field value, or the empty value if missing
getMetadata :: Metadata -> T.Text -> Value
getMetadata m k = fold $ HMap.lookup k m

-- |Add a field value, appending it to any existing value
addMetadata :: Metadata -> T.Text -> Value -> Metadata
addMetadata m k v = HMap.insertWith (flip mappend) k v m

data Document = Document
  { documentID :: T.Text
  , documentCollection :: T.Text
  -- , documentModified :: UTCTime
  , documentMetadata :: Metadata
  } deriving (Show)

instance JSON.ToJSON Document where
  toJSON Document{..} = JSON.Object
    $ HMap.insert "id" (JSON.String documentID)
    $ HMap.insert "collection_ssm" (JSON.String documentCollection)
    $ HMap.fromList $ mapMaybe (\(k, v) -> (,) ("desc_metadata__" <> k <> "_tesim") <$> valueJSON v) $ HMap.toList documentMetadata

instance JSON.FromJSON Document where
  parseJSON = JSON.withObject "document" $ \o -> do
    i <- o JSON..: "id"
    c <- o JSON..: "collection_ssm"
    m <- foldlM (\m (k, v) -> maybe (return m) (\k' -> addMetadata m k' <$> JSON.parseJSON v)
      $ T.stripSuffix "_tesim" =<< T.stripPrefix "desc_metadata__" k) HMap.empty $ HMap.toList o
    return Document
      { documentID = i
      , documentCollection = c
      , documentMetadata = m
      }

-- |Update the metadata within a document
mapMetadata :: (Metadata -> Metadata) -> Document -> Document
mapMetadata f d = d{ documentMetadata = f $ documentMetadata d }

type Documents = V.Vector Document

-- |Convert a handle URL like @http://hdl.handle.net/x/y@ to a 'documentID' like @hdl-handle-net-x-y@.
handleToID :: Monad m => Value -> m T.Text
handleToID (Value [T.stripPrefix "http://hdl.handle.net/" -> Just h]) = return $ "hdl-handle-net-" <> T.map f h where
  f '.' = '-'
  f '/' = '-'
  f '\\' = '-'
  f '?' = '-'
  f '=' = '-'
  f c = c
handleToID v = fail $ "invalid handle: " ++ show v
