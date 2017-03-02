{-# LANGUAGE OverloadedStrings #-}
module Source.DLib
  ( loadDLib
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP

import           Util
import           Document
import           Source.Solr
import           Source.DLTS

dlibRequest :: HTTP.Request
dlibRequest = HTTP.parseRequest_ "http://dlib.nyu.edu"

parseDLib :: T.Text -> T.Text -> JSON.Object -> JSON.Parser Document
parseDLib pfx name o = do
  let t = parseValue <$> HMap.delete "metadata" o
  m <- mapM pm =<< o JSON..: "metadata"
  let tm = t <> m
  hdl <- dltsHandleID $ getMetadata tm "handle"
  return Document
    { documentID = pfx <> T.cons ':' hdl
    , documentCollection = name
    , documentMetadata = tm
    }
  where
  pm v = pv <$> JSON.withObject "dlib metadata" (JSON..: "value") v
  pv (JSON.Object v) = foldMap parseValue $ HMap.lookup "value" v
  pv v = parseValue v

loadDLib :: T.Text -> T.Text -> BS.ByteString -> IO Documents
loadDLib pfx name path = do
  -- actually drupal, but acts like solr
  j <- loadSolr (addRequestPath dlibRequest path) "" mempty
  parseM (mapM $ parseDLib pfx name) j
