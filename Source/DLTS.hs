{-# LANGUAGE OverloadedStrings #-}
module Source.DLTS
  ( DLTSCore(..)
  , loadDLTS
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.HashSet as HSet
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Network.HTTP.Client as HTTP

import           Util
import           Document
import           Source.Solr

data DLTSCore
  = DLTSCore
  | DLTSViewer
  deriving (Eq, Enum, Bounded, Show)

instance JSON.FromJSON DLTSCore where
  parseJSON = JSON.withText "DLTS core" cn where
    cn "core" = return DLTSCore
    cn "viewer" = return DLTSViewer
    cn _ = fail "Unknown DLTS core"

dltsBase :: HTTP.Request
dltsBase = HTTP.parseRequest_ "http://discovery.dlib.nyu.edu:8080/solr3_discovery"

dltsRequest :: DLTSCore -> HTTP.Request
dltsRequest DLTSCore = addRequestPath dltsBase "select"
dltsRequest DLTSViewer = addRequestPath dltsBase "viewer/select"

dltsCollectionFields :: DLTSCore -> (T.Text, T.Text)
dltsCollectionFields DLTSCore = ("collection_code", "collection_title")
dltsCollectionFields DLTSViewer = ("sm_collection_code", "sm_collection_label")

loadDLTS :: DLTSCore -> T.Text -> HSet.HashSet T.Text -> IO Documents
loadDLTS core c fl = parseM (mapM doc) =<< loadSolr (dltsRequest core) (TE.encodeUtf8 $ code <> T.cons ':' c) (fl <> fl') where
  (code, label) = dltsCollectionFields core
  fl' = HSet.fromList ["ss_handle", code, label, "ds_changed"]
  doc o = do
    hdl <- maybe (fail "invalid handle") return . T.stripPrefix "http://hdl.handle.net/2333.1/" =<< o JSON..: "ss_handle"
    cc <- one =<< o JSON..: code
    cl <- one =<< o JSON..: label
    m <- o JSON..: "ds_changed"
    o' <- mapM JSON.parseJSON o
    return Document
      { documentID = cc <> ":hdl-handle-net-2333-1-" <> hdl
      , documentCollection = cl
      , documentModified = m
      , documentMetadata = o'
      }
    where
    one (Value [x]) = return x
    one _ = fail "multiple values"
