{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module FDA
  ( loadFDAIndex
  , loadFDA
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Read as T (decimal)
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP

import           Util
import           Document

data FDAHandle = FDAHandle
  { _fdaHandlePrefix, fdaHandleSuffix :: !Int
  }

parseFDAHandle :: Monad m => T.Text -> m FDAHandle
parseFDAHandle (T.break ('/' ==) -> (T.decimal -> Right (a, ""), T.uncons -> Just ('/', T.decimal -> Right (b, "")))) = return $ FDAHandle a b
parseFDAHandle s = fail $ "invalid FDA handle: " ++ show s

instance JSON.FromJSON FDAHandle where
  parseJSON = JSON.withText "FDA handle" parseFDAHandle

data FDACollection = FDACollection
  { fdaCollectionId :: Int
  , fdaCollectionHandle :: FDAHandle
  , fdaCollectionName :: T.Text
  , fdaCollectionSize :: Int
  }

instance JSON.FromJSON FDACollection where
  parseJSON = JSON.withObject "FDA collection" $ \o -> FDACollection
    <$> o JSON..: "id"
    <*> o JSON..: "handle"
    <*> o JSON..: "name"
    <*> o JSON..: "numberItems"

parseFDA :: T.Text -> JSON.Value -> JSON.Parser Documents
parseFDA name = withArrayOrSingleton $ mapM $ JSON.withObject "FDA item" $ \obj -> do
  FDAHandle hdl0 hdl1 <- obj JSON..: "handle"
  metadata <- readMetadata =<< obj JSON..: "metadata"
  return Document
    { documentID = "fda:hdl-handle-net-" <> (T.pack $ show hdl0) <> "-" <> (T.pack $ show hdl1)
    , documentCollection = name
    , documentMetadata = metadata
    }
  where
  readField = JSON.withObject "FDA.metadata.field" $ \o ->
    (,) <$> o JSON..: "key" <*> o JSON..: "value"
  readMetadata = JSON.withArray "FDA.metadata" $
    V.foldM (\m f -> uncurry (addMetadata m) <$> readField f) mempty

fdaRequest :: HTTP.Request
fdaRequest = HTTP.addRequestHeader HTTP.hAccept "application/json"
  $ HTTP.parseRequest_ "https://archive.nyu.edu/rest/collections"

loadFDAIndex :: Int -> IO (HM.HashMap Int Int)
loadFDAIndex z =
  V.foldl' (\m c -> HM.insert (fdaHandleSuffix $ fdaCollectionHandle c) (fdaCollectionId c) m) HM.empty
    . HTTP.getResponseBody <$> HTTP.httpJSON (HTTP.setQueryString [("limit",Just $ BSC.pack $ show z)] fdaRequest)

loadFDA :: Int -> IO Documents
loadFDA i = do
  c <- HTTP.getResponseBody <$> HTTP.httpJSON req
  j <- HTTP.getResponseBody <$> HTTP.httpJSON (
    HTTP.setQueryString [("expand",Just "metadata"),("limit",Just $ BSC.pack $ show $ fdaCollectionSize c)]
    $ addRequestPath req "items")
  either fail return $ JSON.parseEither (parseFDA $ fdaCollectionName c) j
  where req = addRequestPath fdaRequest (BSC.pack $ show i)
