{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Source.DLTS
  ( DLTSCore(..)
  , loadDLTS
  ) where

import           Control.Monad (guard)
import qualified Data.Aeson.Types as JSON
import qualified Data.HashSet as HSet
import           Data.Maybe (fromMaybe)
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

dltsBase :: HTTP.Request
dltsBase = HTTP.parseRequest_ "http://discovery.dlib.nyu.edu:8080/solr3_discovery"

data DLTSCoreMeta = DLTSCoreMeta
  { dltsRequest :: !HTTP.Request
  , dltsCollectionCode, dltsCollectionName, dltsHandle, dltsChanged :: !T.Text
  }

dltsCoreMeta :: DLTSCore -> DLTSCoreMeta
dltsCoreMeta DLTSCore   = DLTSCoreMeta (addRequestPath dltsBase "select")
  "collection_code" "collection_title" "ss_handle" "ds_changed"
dltsCoreMeta DLTSViewer = DLTSCoreMeta (addRequestPath dltsBase "viewer/select")
  "sm_collection_code" "sm_collection_label" "ss_handle" "ds_changed"
dltsCoreMeta NYUPress   = DLTSCoreMeta (addRequestPath dltsBase "nyupress/select")
  "collection_code" "publisher" "handle" "timestamp"

loadDLTS :: T.Text -> Maybe T.Text -> DLTSCore -> T.Text -> HSet.HashSet T.Text -> IO Documents
loadDLTS pfx name core c fl = parseM (mapM doc) =<< loadSolr dltsRequest (TE.encodeUtf8 $ dltsCollectionCode <> T.cons ':' c) (fl <> fl') where
  DLTSCoreMeta{..} = dltsCoreMeta core
  fl' = HSet.fromList [dltsCollectionName, dltsHandle]
  doc o = do
    hdl <- handleToID =<< o JSON..: dltsHandle
    i <- o JSON..:? "id"
    cl <- oneValue =<< o JSON..: dltsCollectionName
    -- mtime <- o JSON..: dltsChanged
    o' <- mapM JSON.parseJSON o
    return $ mkDocument
      (pfx <> T.cons ':' (fromMaybe hdl $ guard (core == NYUPress) >> i))
      (fromMaybe cl name)
      o'
