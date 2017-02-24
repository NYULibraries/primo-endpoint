{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module FDA
  ( loadFDA
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isDigit)
import qualified Data.HashMap.Lazy as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP

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
  , ("identifier.citation", "citation") -- also "relation", "identifier"
  , ("identifier.uri",      "identifier") -- also "available"
  , ("publisher",           "publisher")
  , ("publisher.place",     publisherPlace) -- merged into "publisher"
  , ("rights",              "rights")
  , ("subject",             "subject")
  , ("title",               "title")
  ]

processMetadata :: Metadata -> Metadata
processMetadata = processPublisher . processIdentifier where
  processIdentifier =
    dup "identifier" "citation"
    . dup "relation" "citation"
    . dup "available" "identifier"
  processPublisher m = HM.adjust (\(Value pl) -> Value
    [ w <> ":" <> p <> "," <> d | p <- pl, w <- get m publisherPlace, d <- get m "date" ]) "publisher"
    $ HM.delete publisherPlace m
  get m k = values $ HM.lookupDefault (Value [T.empty]) k m
  dup d s m = maybe m (\i -> HM.insertWith (flip mappend) d i m) $ HM.lookup s m

data FDACollection = FDACollection
  { collectionName :: T.Text
  , collectionSize :: Int
  }

instance JSON.FromJSON FDACollection where
  parseJSON = JSON.withObject "FDA collection" $ \o -> do
    n <- o JSON..: "name"
    c <- o JSON..: "numberItems"
    return FDACollection
      { collectionName = n
      , collectionSize = c
      }

parseFDA :: T.Text -> JSON.Value -> JSON.Parser [Document]
parseFDA name = oneOrMany $ JSON.withObject "FDA" $ \obj -> do
  (handle0, handle1) <- readHandle =<< obj JSON..: "handle"
  metadata <- readMetadata =<< obj JSON..: "metadata"
  return Document
    { documentID = "fda:hdl-handle-net-" <> handle0 <> "-" <> handle1
    , documentType = "Report"
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

loadFDA :: Int -> IO [Document]
loadFDA i = do
  req <- HTTP.addRequestHeader HTTP.hAccept "application/json"
    <$> HTTP.parseRequest ("https://archive.nyu.edu/rest/collections/" ++ show i)
  FDACollection n z <- HTTP.getResponseBody <$> HTTP.httpJSON req
  j <- HTTP.getResponseBody <$> HTTP.httpJSON (
    HTTP.setQueryString [("expand",Just "metadata"),("limit",Just $ BSC.pack $ show z)]
    req{ HTTP.path = HTTP.path req <> "/items" })
  either fail return $ JSON.parseEither (parseFDA n) j
