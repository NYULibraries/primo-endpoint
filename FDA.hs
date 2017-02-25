{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module FDA
  ( loadFDA
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isDigit)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP

import           Util
import           Document

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

parseFDA :: T.Text -> JSON.Value -> JSON.Parser Documents
parseFDA name = withArrayOrSingleton $ mapM $ JSON.withObject "FDA item" $ \obj -> do
  (handle0, handle1) <- readHandle =<< obj JSON..: "handle"
  metadata <- readMetadata =<< obj JSON..: "metadata"
  return Document
    { documentID = "fda:hdl-handle-net-" <> handle0 <> "-" <> handle1
    , documentCollection = name
    , documentMetadata = metadata
    }
  where
  readHandle (T.break ('/' ==) -> (a@(T.all isDigit -> True), T.uncons -> Just ('/', b@(T.all isDigit -> True)))) = return (a, b)
  readHandle s = fail $ "invalid FDA handle: " ++ show s
  readField = JSON.withObject "FDA.metadata.field" $ \o ->
    (,) <$> o JSON..: "key" <*> o JSON..: "value"
  readMetadata = JSON.withArray "FDA.metadata" $
    V.foldM (\m f -> uncurry (addMetadata m) <$> readField f) mempty

loadFDA :: Int -> IO Documents
loadFDA i = do
  req <- HTTP.addRequestHeader HTTP.hAccept "application/json"
    <$> HTTP.parseRequest ("https://archive.nyu.edu/rest/collections/" ++ show i)
  FDACollection n z <- HTTP.getResponseBody <$> HTTP.httpJSON req
  j <- HTTP.getResponseBody <$> HTTP.httpJSON (
    HTTP.setQueryString [("expand",Just "metadata"),("limit",Just $ BSC.pack $ show z)]
    req{ HTTP.path = HTTP.path req <> "/items" })
  either fail return $ JSON.parseEither (parseFDA n) j
