{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  | NYUPress
  deriving (Eq, Enum, Bounded, Show)

instance JSON.FromJSON DLTSCore where
  parseJSON = JSON.withText "DLTS core" cn where
    cn "core" = return DLTSCore
    cn "viewer" = return DLTSViewer
    cn "nyupress" = return NYUPress
    cn _ = fail "Unknown DLTS core"

solr3Base :: HTTP.Request
solr3Base = HTTP.parseRequest_ "http://discovery.dlib.nyu.edu:8080/solr3_discovery"

solr6Base :: HTTP.Request
solr6Base = HTTP.parseRequest_ "http://discovery1.dlib.nyu.edu:8983/solr"


data DLTSCoreMeta = DLTSCoreMeta
  { dltsRequest :: !HTTP.Request
  , dltsCollectionCode :: !T.Text
  }

dltsCoreMeta :: DLTSCore -> DLTSCoreMeta
dltsCoreMeta DLTSCore   = DLTSCoreMeta (addRequestPath solr3Base "select") "collection_code"
dltsCoreMeta DLTSViewer = DLTSCoreMeta (addRequestPath solr6Base "viewer/select") "sm_collection_code"
dltsCoreMeta NYUPress   = DLTSCoreMeta (addRequestPath solr3Base "nyupress/select") "collection_code"

loadDLTS :: DLTSCore -> T.Text -> HSet.HashSet T.Text -> IO Documents
loadDLTS core c fl = parseM (mapM $ mapM JSON.parseJSON)
  =<< loadSolr dltsRequest (TE.encodeUtf8 $ dltsCollectionCode <> T.cons ':' c) fl where
  DLTSCoreMeta{..} = dltsCoreMeta core
    -- mtime <- o JSON..: (if core == NYUPress then "timestamp" else "ds_changed")
