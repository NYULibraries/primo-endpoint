{-# LANGUAGE OverloadedStrings #-}
module Server
  ( server
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Network.HTTP.Types (ok200, notFound404, methodNotAllowed405, hAccept, hContentType, hLastModified, hDate)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Log

import           Config
import           Cache

resolvePath :: Config -> Wai.Request -> Maybe (FilePath, UTCTime -> IO UTCTime)
resolvePath conf req = case Wai.pathInfo req of
  [] ->
    Just (configCache conf, updateCollections force conf)
  [s] | Just c <- HM.lookup s (configCollections conf) ->
    Just (collectionCache c, updateCollection force c)
  _ -> Nothing
  where
  force = Just (Just "1") == lookup "force" (Wai.queryString req)

formatDate :: UTCTime -> BSC.ByteString
formatDate = BSC.pack . formatTime defaultTimeLocale "%a, %d %b %Y %T GMT"

server :: Int -> Bool -> Config -> IO ()
server port logging conf = Warp.run port $ middleware $ \req resp -> resp =<<
  maybe
    (return $ Wai.responseLBS notFound404 [] mempty)
    (\(f, u) -> case Wai.requestMethod req of
      "GET" -> do
        t <- getCurrentTime
        m <- u t
        return $ Wai.responseFile ok200
          [ (hContentType, "application/json")
          , (hLastModified, formatDate m)
          , (hDate, formatDate t)
          ] f Nothing
      _ -> return $ Wai.responseLBS methodNotAllowed405 [(hAccept, "GET")] mempty
    )
    (resolvePath conf req)
  where
  middleware = (if logging then Log.logStdout else id)
