{-# LANGUAGE OverloadedStrings #-}
module Source.DLib
  ( loadDLib
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import           Data.Monoid ((<>))
import qualified Network.HTTP.Client as HTTP

import           Util
import           Document
import           Source.Solr

dlibRequest :: HTTP.Request
dlibRequest = HTTP.parseRequest_ "http://dlib.nyu.edu"

parseDLib :: JSON.Object -> JSON.Parser Document
parseDLib o = do
  let t = parseValue <$> HMap.delete "metadata" o
  m <- mapM pm =<< o JSON..: "metadata"
  return $ t <> m
  where
  pm v = pv <$> JSON.withObject "dlib metadata" (JSON..: "value") v
  pv (JSON.Object v) = foldMap parseValue $ HMap.lookup "value" v
  pv v = parseValue v

loadDLib :: BS.ByteString -> IO Documents
loadDLib path =
  parseM (mapM parseDLib)
    =<< loadDrupal (addRequestPath dlibRequest path)
