{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Source.Solr
  ( loadSolr
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashSet as HSet
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Data.Text.Read as TR (decimal)
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

data SolrResponse = SolrResponse
  { solrStart, solrNumFound :: Int
  , solrDocs :: V.Vector JSON.Object
  } deriving (Show)

parseInt :: JSON.Value -> JSON.Parser Int
parseInt (JSON.String (TR.decimal -> Right (n, ""))) = return n -- drupal can have a string here
parseInt v = JSON.parseJSON v

instance JSON.FromJSON SolrResponse where
  parseJSON = JSON.withObject "Solr response" $ \o -> do
    r <- o JSON..: "response"
    s <- parseInt =<< r JSON..: "start"
    n <- parseInt =<< r JSON..: "numFound"
    d <- r JSON..: "docs" 
    return SolrResponse
      { solrStart = s
      , solrNumFound = n
      , solrDocs = d
      }

loadSolrOffset :: HTTP.Request -> BSC.ByteString -> BSC.ByteString -> Int -> Int -> IO SolrResponse
loadSolrOffset req fq fl start rows =
  HTTP.responseBody <$> HTTP.httpJSON
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
loadSolr req fq fl = loop 0 100 where
  loop _ 0 = return mempty
  loop start rows = do
    SolrResponse s n d <- loadSolrOffset req fq fl' start rows
    let s' = s + length d
    (d <>) <$> loop s' (n-s')
  fl' = BSC.intercalate "," $ map TE.encodeUtf8 $ HSet.toList fl
