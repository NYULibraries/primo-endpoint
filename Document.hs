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
  , valueJSON
  , keyValueJSON
  , valueJSON'
  , keyValueJSON'
  , valueOr
  , Metadata
  , getMetadata
  , addMetadata
  , parseNestedMetadata
  , Document
  , Documents
  , handleToID
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as HMap
import           Data.Foldable (fold)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (ParseTime(..), formatTime)
import qualified Data.Vector as V
import           Network.URI (parseURI, uriScheme)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

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

-- |Nothing for empty values
maybeValue :: Value -> Maybe Value
maybeValue (Value []) = Nothing
maybeValue v = Just v

-- |The alternative monoid append, left-biased
valueOr :: Value -> Value -> Value
valueOr (Value []) a = a
valueOr a _ = a

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
  toEncoding (Value l) = JSON.foldable l

-- |'JSON.toJSON' for non-empty values
valueJSON :: Value -> Maybe JSON.Value
valueJSON = fmap JSON.toJSON . maybeValue

-- |'JSON..=' for non-empty values
keyValueJSON :: (Monoid kv, JSON.KeyValue kv) => T.Text -> Value -> kv
keyValueJSON k = foldMap (k JSON..=) . maybeValue

-- |'valueJSON' but single values as scalars
valueJSON' :: Value -> Maybe JSON.Value
valueJSON' (Value [v]) = Just $ JSON.String v
valueJSON' v = valueJSON v

-- |'keyValueJSON' but single values as scalars
keyValueJSON' :: (Monoid kv, JSON.KeyValue kv) => T.Text -> Value -> kv
keyValueJSON' k (Value [v]) = k JSON..= v
keyValueJSON' k v = keyValueJSON k v

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

valueHTML :: T.Text -> H.Html
valueHTML s
  | Just u <- parseURI (T.unpack s), uriScheme u `elem` ["http:", "https:", "ftp:"] =
    H.a H.! HA.href (H.stringValue $ show u) $ H.text s
  | otherwise = H.text s

instance H.ToMarkup Value where
  toMarkup (Value []) = mempty
  toMarkup (Value [s]) = valueHTML s
  toMarkup (Value l) = H.ul $ foldMap (H.li . valueHTML) l

type Metadata = HMap.HashMap T.Text Value

-- |Lookup a single field value, or the empty value if missing
getMetadata :: Metadata -> T.Text -> Value
getMetadata m k = HMap.lookupDefault mempty k m

-- |Add a field value, appending it to any existing value
addMetadata :: Metadata -> T.Text -> Value -> Metadata
addMetadata m k v = HMap.insertWith (flip mappend) k v m

-- |Parse an arbitrary structure into flattened 'Metadata', concatenating in arrays, and creating sub-keys in objects by joining with the given separator.
-- Any produced fields are prefixed with the second argument (followed by a separator if non-null).
parseNestedMetadata :: T.Text -> T.Text -> JSON.Value -> Metadata
parseNestedMetadata _ _ JSON.Null = HMap.empty
parseNestedMetadata sep pfx (JSON.Object o) = HMap.foldrWithKey (\k ->
  HMap.unionWith mappend . parseNestedMetadata sep (pfx' <> k)) mempty o
  where pfx' | T.null pfx = pfx
             | otherwise = pfx <> sep
parseNestedMetadata sep pfx (JSON.Array l) = V.foldr
  (HMap.unionWith mappend . parseNestedMetadata sep pfx) mempty l
parseNestedMetadata _ k v = HMap.singleton k $ parseValue v

type Document = Metadata
type Documents = V.Vector Document

-- |Convert a handle URL like @http://hdl.handle.net/x/y@ to a 'documentID' like @hdl-handle-net-x-y@.
handleToID :: Monad m => T.Text -> m T.Text
handleToID (T.stripPrefix "http://hdl.handle.net/" -> Just h) = return $ "hdl-handle-net-" <> T.map f h where
  f '.' = '-'
  f '/' = '-'
  f '\\' = '-'
  f '?' = '-'
  f '=' = '-'
  f c = c
handleToID v = fail $ "invalid handle: " ++ show v
