{-# LANGUAGE OverloadedStrings #-}
module Server
  ( server
  ) where

import           Network.HTTP.Types (ok200, methodNotAllowed405, hAccept, hContentType)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

server :: Int -> FilePath -> IO ()
server port file = Warp.run port $ \req resp -> resp $ do
  case Wai.requestMethod req of
    "GET" -> Wai.responseFile ok200 [(hContentType, "application/json")] file Nothing
    _ -> Wai.responseLBS methodNotAllowed405 [(hAccept, "GET")] mempty
