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

parseInt :: JSON.Value -> JSON.Parser Int
parseInt (JSON.String (TR.decimal -> Right (n, ""))) = return n -- drupal can have a string here
parseInt v = JSON.parseJSON v

parseSolr :: JSON.Value -> JSON.Parser (V.Vector JSON.Object, Maybe Int)
parseSolr = JSON.withObject "Solr response" $ \o -> do
  r <- o JSON..: "response"
  s <- parseInt =<< r JSON..: "start"
  n <- parseInt =<< r JSON..: "numFound"
  d <- r JSON..: "docs" 
  let s' = s + V.length d
  return (d, if s' >= n || V.null d then Nothing else Just s')

loadSolrOffset :: HTTP.Request -> BSC.ByteString -> BSC.ByteString -> Int -> Int -> IO (V.Vector JSON.Object, Maybe Int)
loadSolrOffset req fq fl start rows =
  parseM parseSolr . HTTP.responseBody =<< HTTP.httpJSON
    (HTTP.setQueryString
      [ ("wt", Just "json")
      , ("hl", Just "off")
      , ("timeAllowed", Just "0")
      , ("fl", Just fl)
      , ("fq", Just fq)
      , ("sort", Just "id asc")
      , ("start", Just $ BSC.pack $ show start)
      , ("rows", Just $ BSC.pack $ show rows)
      ] req)

loadSolr :: HTTP.Request -> BSC.ByteString -> HSet.HashSet T.Text -> IO (V.Vector JSON.Object)
loadSolr req fq fl = V.concat <$> loop 0 where
  rows = 1000
  loop start = do
    (d, m) <- loadSolrOffset req fq fl' start rows
    (d :) <$> maybe (return []) loop m
  fl' = BSC.intercalate "," $ map TE.encodeUtf8 $ HSet.toList fl

-- Drupal acts like solr, some fields are just unused
loadDrupal :: HTTP.Request -> IO (V.Vector JSON.Object)
loadDrupal req = loadSolr req BSC.empty HSet.empty
