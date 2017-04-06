{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Source.FDA
  ( loadFDAIndex
  , loadFDA
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Text.Read as T (decimal)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (LocalTime, getTimeZone, localTimeToUTC, utc)
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP
import           System.IO.Unsafe (unsafePerformIO)

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

_guessLocalTime :: LocalTime -> UTCTime
_guessLocalTime l = unsafePerformIO $ do
  z <- getTimeZone $ localTimeToUTC utc l
  z' <- getTimeZone $ localTimeToUTC z l
  return $ localTimeToUTC z' l

parseFDA :: FDACollection -> JSON.Value -> JSON.Parser Documents
parseFDA c = withArrayOrSingleton $ mapM $ JSON.withObject "FDA item" $ \obj -> do
  handle <- obj JSON..: "handle"
  -- any other fields from the top level we want?  more general list?
  -- mtime <- _guessLocalTime <$> obj JSON..: "lastModified"
  metadata <- readMetadata =<< obj JSON..: "metadata"
  return
    $ HMap.insert "parentCollection.name" (value $ fdaCollectionName c)
    $ HMap.insert "handle" handle
    metadata
  where
  readField = JSON.withObject "FDA.metadata.field" $ \o ->
    (,) <$> o JSON..: "key" <*> o JSON..: "value"
  readMetadata = JSON.withArray "FDA.metadata" $
    V.foldM (\m f -> uncurry (addMetadata m) <$> readField f) mempty

fdaRequest :: HTTP.Request
fdaRequest = HTTP.addRequestHeader HTTP.hAccept "application/json"
  $ HTTP.parseRequest_ "https://archive.nyu.edu/rest/collections"

loadFDAIndex :: Int -> IO (HMap.HashMap Int Int)
loadFDAIndex 0 = return HMap.empty
loadFDAIndex z =
  V.foldl' (\m c -> HMap.insert (fdaHandleSuffix $ fdaCollectionHandle c) (fdaCollectionId c) m) HMap.empty
    . HTTP.responseBody <$> HTTP.httpJSON (HTTP.setQueryString [("limit",Just $ BSC.pack $ show z)] fdaRequest)

loadFDA :: Int -> IO Documents
loadFDA i = do
  c <- HTTP.responseBody <$> HTTP.httpJSON req
  j <- HTTP.responseBody <$> HTTP.httpJSON (
    HTTP.setQueryString [("expand",Just "metadata"),("limit",Just $ BSC.pack $ show $ fdaCollectionSize c)]
    $ addRequestPath req "items")
  parseM (parseFDA c) j
  where req = addRequestPath fdaRequest (BSC.pack $ show i)
