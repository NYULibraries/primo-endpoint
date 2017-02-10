{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module FDA
  ( readFDA
  , sourceFDA
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import           Data.Char (isDigit)
import qualified Data.HashMap.Lazy as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V

import           Document

oneOrMany :: (JSON.Value -> JSON.Parser a) -> JSON.Value -> JSON.Parser [a]
oneOrMany p (JSON.Array l) = mapM p $ V.toList l
oneOrMany p a = return <$> p a

-- |Temporary name for publisher.place holding field
publisherPlace :: T.Text
publisherPlace = "publisher.place"

fieldMap :: T.Text -> Maybe T.Text
fieldMap k = HM.lookup k $ HM.fromList
  [ ("contributor.author" , "creator")
  , ("date.issued",         "date") -- also merged into "publisher"
  , ("description",         "description")
  , ("format",              "format")
  , ("identifier.citation", "relation") -- also "identifier"
  , ("identifier.uri",      "identifier")
  , ("publisher",           "publisher")
  , ("publisher.place",     publisherPlace) -- merged into "publisher"
  , ("rights",              "rights")
  , ("subject",             "subject")
  , ("title",               "title")
  ]

processMetadata :: Metadata -> Metadata
processMetadata = processPublisher . processIdentifier where
  processIdentifier m = maybe m (\i -> HM.insertWith (flip mappend) "identifier" i m) $ HM.lookup "relation" m
  processPublisher m = HM.adjust (\(Value pl) -> Value
    [ w <> ":" <> p <> "," <> d | p <- pl, w <- get m publisherPlace, d <- get m "date" ]) "publisher"
    $ HM.delete publisherPlace m
  get m k = values $ HM.lookupDefault (Value [T.empty]) k m

readFDA :: JSON.Value -> JSON.Parser [Document]
readFDA = oneOrMany $ JSON.withObject "FDA" $ \obj -> do
  (handle0, handle1) <- readHandle =<< obj JSON..: "handle"
  name <- obj JSON..: "name"
  metadata <- readMetadata =<< obj JSON..: "metadata"
  return $ Document
    { documentID = "fda:hdl-handle-net-" <> handle0 <> "-" <> handle1
    , documentCollection = name
    , documentMetadata = processMetadata metadata
    }
  where
  readHandle (T.break ('/' ==) -> (a@(T.all isDigit -> True), T.uncons -> Just ('/', b@(T.all isDigit -> True)))) = return (a, b)
  readHandle s = fail $ "invalid FDA handle: " ++ show s
  readField :: JSON.Value -> JSON.Parser (Maybe (T.Text, T.Text))
  readField = JSON.withObject "FDA.metadata.field" $ \o -> do
    (T.stripPrefix "dc." -> Just key) <- o JSON..: "key"
    mapM (\f -> (,) f <$> o JSON..: "value") $ fieldMap key
  readMetadata = JSON.withArray "FDA.metadata" $
    V.foldM (\m f -> maybe m (uncurry $ addMetadata m) <$> readField f) mempty

sourceFDA :: String -> String
sourceFDA i@(all isDigit -> True) = "https://archive.nyu.edu/rest/collections/" ++ i ++ "/items?expand=metadata"
sourceFDA s = s
