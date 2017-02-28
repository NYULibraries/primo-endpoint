{-# LANGUAGE OverloadedStrings #-}
module Solr
  ( loadSolr
  ) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BSC
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

data SolrResponse = SolrResponse
  { solrStart, solrNumFound :: Int
  , solrDocs :: V.Vector JSON.Object
  }

instance JSON.FromJSON SolrResponse where
  parseJSON = JSON.withObject "Solr response" $ \o -> do
    r <- o JSON..: "response"
    s <- r JSON..: "start"
    n <- r JSON..: "numFound"
    d <- r JSON..: "docs" 
    return SolrResponse
      { solrStart = s
      , solrNumFound = n
      , solrDocs = d
      }

loadSolrOffset :: HTTP.Request -> BSC.ByteString -> Int -> Int -> IO SolrResponse
loadSolrOffset req fq start rows =
  HTTP.getResponseBody <$> HTTP.httpJSON
    (HTTP.setQueryString
      [ ("wt", Just "json")
      , ("hl", Just "off")
      , ("fq", Just fq)
      , ("sort", Just "id asc")
      , ("start", Just $ BSC.pack $ show start)
      , ("rows", Just $ BSC.pack $ show rows)
      ] req)

loadSolr :: HTTP.Request -> BSC.ByteString -> IO (V.Vector JSON.Object)
loadSolr req fq = loop 0 100 where
  loop _ 0 = return mempty
  loop start rows = do
    SolrResponse s n d <- loadSolrOffset req fq start rows
    let s' = s + length d
    (d <>) <$> loop s' (n-s')
