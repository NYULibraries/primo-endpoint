{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Source.Blacklight
  ( loadBlacklight
  ) where

import           Control.Arrow ((***))
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

import           Util
import           Document

parseBlacklightPage :: JSON.Value -> JSON.Parser (V.Vector Metadata, Bool)
parseBlacklightPage = JSON.withObject "blacklight response" $ \o -> do
  r <- o JSON..: "response"
  p <- r JSON..: "pages"
  l <- p JSON..: "last_page?"
  d <- r JSON..: "docs"
  return (d, l || V.null d)

loadBlacklightPage :: HTTP.Request -> [(BSC.ByteString, BSC.ByteString)] -> BSC.ByteString -> Int -> IO (V.Vector Metadata, Bool)
loadBlacklightPage req fq key page =
  parseM parseBlacklightPage . HTTP.responseBody =<< HTTP.httpJSON
    (HTTP.setQueryString
      ([ ("format", Just "json")
      , ("sort", Just $ key <> " asc")
      , ("rows", Just "1000")
      , ("page", Just $ BSC.pack $ show page)
      ] ++ map (("f[" <>) . (`BSC.snoc` ']') *** Just) fq)
      req)

loadBlacklight :: HTTP.Request -> [(BSC.ByteString, BSC.ByteString)] -> BSC.ByteString -> IO (V.Vector Metadata)
loadBlacklight req fq key = V.concat <$> loop 1 where
  loop page = do
    (d, l) <- loadBlacklightPage req fq key page
    (d :) <$> if l then return [] else loop (succ page)
