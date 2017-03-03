{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Source.Solr
  ( loadSolr
  , loadDrupal
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashSet as HSet
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Data.Text.Read as TR (decimal)
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

import           Util
import           Paginate

parseInt :: JSON.Value -> JSON.Parser Int
parseInt (JSON.String (TR.decimal -> Right (n, ""))) = return n -- drupal can have a string here
parseInt v = JSON.parseJSON v

parseSolrPage :: JSON.Value -> JSON.Parser (V.Vector JSON.Object, Int)
parseSolrPage = JSON.withObject "Solr response" $ \o -> do
  r <- o JSON..: "response"
  n <- parseInt =<< r JSON..: "numFound"
  d <- r JSON..: "docs"
  return (d, n)

loadSolrPage :: HTTP.Request -> BSC.ByteString -> BSC.ByteString -> Int -> Int -> IO (V.Vector JSON.Object, Int)
loadSolrPage req fq fl start rows =
  parseM parseSolrPage . HTTP.responseBody =<< HTTP.httpJSON
    (HTTP.setQueryString
      [ ("wt", Just "json")
      , ("hl", Just "off")
      , ("fl", Just fl)
      , ("fq", Just fq)
      , ("sort", Just "id asc")
      , ("start", Just $ BSC.pack $ show start)
      , ("rows", Just $ BSC.pack $ show rows)
      ] req)

loadSolr :: HTTP.Request -> BSC.ByteString -> HSet.HashSet T.Text -> IO (V.Vector JSON.Object)
loadSolr req fq fl = joinPages (loadSolrPage req fq fl') where
  fl' = BSC.intercalate "," $ map TE.encodeUtf8 $ HSet.toList fl

-- Drupal acts like solr, some fields are just unused
loadDrupal :: HTTP.Request -> IO (V.Vector JSON.Object)
loadDrupal req = loadSolr req BSC.empty HSet.empty
