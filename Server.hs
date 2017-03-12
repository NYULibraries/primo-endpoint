{-# LANGUAGE OverloadedStrings #-}
module Server
  ( server
  ) where

import           Control.Monad (join, mfilter)
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe (mapMaybe)
import qualified Data.Text.Encoding as TE (decodeUtf8)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Network.HTTP.Types (ok200, notFound404, methodNotAllowed405, hAccept, hContentType, hLastModified, hDate)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Log
import           Network.Wai.Parse (parseHttpAccept)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import           Config
import           Cache
import           View

formatDate :: UTCTime -> BSC.ByteString
formatDate = BSC.pack . formatTime defaultTimeLocale "%a, %d %b %Y %T GMT"

serve :: Config -> Wai.Request -> IO Wai.Response
serve conf req =
  maybe
    (return $ Wai.responseLBS notFound404 [] mempty)
    (\(f, u) -> case Wai.requestMethod req of
      "GET" -> do
        t <- getCurrentTime
        m <- u (boolq "refresh") t
        if html
          then Wai.responseBuilder ok200 [(hContentType, "text/html;charset=utf-8")]
            . renderHtmlBuilder <$> view conf (join collid) f (Wai.queryString req)
          else return $ Wai.responseFile ok200
            [ (hContentType, "application/json")
            , (hLastModified, formatDate m)
            , (hDate, formatDate t)
            ] f Nothing
      _ -> return $ Wai.responseLBS methodNotAllowed405
        [(hAccept, "GET")] mempty)
    coll
  where
  query = Wai.queryString req
  getq k = lookup k query
  boolq k = case getq k of
    Nothing -> False
    Just Nothing -> True
    Just (Just "1") -> True
    Just (Just "true") -> True
    _ -> False
  collid = case Wai.pathInfo req of
    [] -> return $ fmap TE.decodeUtf8 $ mfilter (not . BSC.null) $ join $ getq "collection"
    [x] -> return $ Just x
    _ -> Nothing
  coll = getCollection conf =<< collid
  accept = foldMap parseHttpAccept $ lookup hAccept $ Wai.requestHeaders req
  html = not (boolq "json") && (boolq "html" ||
    head (mapMaybe (\t -> case t of
      "text/html" -> Just True
      "application/json" -> Just False
      _ -> Nothing) accept ++ [False]))

server :: Int -> Bool -> Config -> IO ()
server port logging conf = Warp.run port
  $ (if logging then Log.logStdout else id)
  $ (>>=) . serve conf
