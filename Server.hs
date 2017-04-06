{-# LANGUAGE OverloadedStrings #-}
module Server
  ( server
  ) where

import           Control.Monad (guard)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HMap
import           Data.Maybe (mapMaybe)
import qualified Data.Text.Encoding as TE (decodeUtf8)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Network.HTTP.Types (badRequest400, notFound404, methodNotAllowed405, hAccept, hLastModified, hDate)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Log
import           Network.Wai.Parse (parseHttpAccept)

import           Config
import           Collection
import           Output

formatDate :: UTCTime -> BSC.ByteString
formatDate = BSC.pack . formatTime defaultTimeLocale "%a, %d %b %Y %T GMT"

serve :: Config -> Wai.Request -> IO Wai.Response
serve conf req = maybe
  (return $ Wai.responseLBS (if Wai.pathInfo req == [] then badRequest400 else notFound404) [] mempty)
  (\c -> case Wai.requestMethod req of
    "GET" -> do
      -- load documents (possibly in "orig", untranslated form)
      t <- getCurrentTime
      let t' = if refresh then Nothing else Just t
      d <- maybe
        (generateCollection conf t' c)
        (loadCollection conf t')
        $ guard orig >> c
      return $ Wai.mapResponseHeaders (++
        [ (hLastModified, formatDate t) -- XXX
        , (hDate, formatDate t)
        ]) $ outputResponse fmt conf c (Wai.queryString req) d
    _ -> return $ Wai.responseLBS methodNotAllowed405
      [(hAccept, "GET")] mempty)
  coll
  where
  query = Wai.queryString req
  getq k = lookup k query
  boolq k = case getq k of
    Just Nothing -> True
    Just (Just "1") -> True
    Just (Just "on") -> True
    Just (Just "true") -> True
    _ -> False
  coll = case (Wai.pathInfo req, getq "collection") of
    ([c], _) -> getcoll c
    ([], Just (Just c)) | not (BSC.null c) -> getcoll $ TE.decodeUtf8 c
    ([], _) -> Just Nothing -- all
    _ -> Nothing
  getcoll c = Just <$> HMap.lookup c (configCollections conf)
  refresh = boolq "refresh"
  orig = boolq "orig"
  fmt
    | boolq "json" = OutputPrimo
    | boolq "html" = OutputHTML orig
    | Just f <- case getq "fmt" of
      Just (Just "html") -> Just $ OutputHTML orig
      Just (Just "json") -> Just OutputPrimo
      Just (Just "primo") -> Just OutputPrimo
      Just (Just "xml") -> Just OutputMODS
      Just (Just "mods") -> Just OutputMODS
      _ -> Nothing = f
    | (f:_) <- mapMaybe fmtmt $ foldMap parseHttpAccept $ lookup hAccept $ Wai.requestHeaders req = f
    | otherwise = OutputPrimo
  fmtmt "application/json" = Just OutputPrimo
  fmtmt "text/json" = Just OutputPrimo
  fmtmt "text/html" = Just $ OutputHTML orig
  fmtmt "application/xml" = Just OutputMODS
  fmtmt "text/xml" = Just OutputMODS
  fmtmt _ = Nothing

server :: Int -> Bool -> Config -> IO ()
server port logging conf = Warp.run port
  $ (if logging then Log.logStdout else id)
  $ (>>=) . serve conf
