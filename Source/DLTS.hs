{-# LANGUAGE OverloadedStrings #-}
module Source.DLTS
  ( loadDLTS
  ) where

import qualified Data.Aeson.Types as JSON
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP

import           Util
import           Document
import           Source.Solr

dltsRequest :: HTTP.Request
dltsRequest = HTTP.parseRequest_ "http://discovery.dlib.nyu.edu:8080/solr3_discovery/viewer/select"

loadDLTS :: T.Text -> IO Documents
loadDLTS c = parseM (mapM doc) =<< loadSolr dltsRequest ("sm_collection_code:" <> TE.encodeUtf8 c) where
  doc o = do
    i <- o JSON..: "id"
    cc <- one =<< o JSON..: "sm_collection_code"
    cl <- one =<< o JSON..: "sm_collection_label"
    m <- o JSON..: "ds_changed"
    o' <- mapM JSON.parseJSON o
    return Document
      { documentID = cc <> ":" <> T.replace "/" "-" i
      , documentCollection = cl
      , documentModified = m
      , documentMetadata = o'
      }
    where
    one x
      | V.length x == 1 = return $ V.head x
      | otherwise = fail "multiple values"
