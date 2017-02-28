{-# LANGUAGE OverloadedStrings #-}
module Source.DLTS
  ( loadDLTS
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.HashSet as HSet
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

loadDLTS :: T.Text -> HSet.HashSet T.Text -> IO Documents
loadDLTS c fl = parseM (mapM doc) =<< loadSolr dltsRequest ("sm_collection_code:" <> TE.encodeUtf8 c) (fl <> fl') where
  fl' = HSet.fromList ["ss_handle", "sm_collection_code", "sm_collection_label", "ds_changed"]
  doc o = do
    hdl <- maybe (fail "invalid handle") return . T.stripPrefix "http://hdl.handle.net/2333.1/" =<< o JSON..: "ss_handle"
    cc <- one =<< o JSON..: "sm_collection_code"
    cl <- one =<< o JSON..: "sm_collection_label"
    m <- o JSON..: "ds_changed"
    o' <- mapM JSON.parseJSON o
    return Document
      { documentID = cc <> ":hdl-handle-net-2333-1-" <> hdl
      , documentCollection = cl
      , documentModified = m
      , documentMetadata = o'
      }
    where
    one x
      | V.length x == 1 = return $ V.head x
      | otherwise = fail "multiple values"
