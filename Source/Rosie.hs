{-# LANGUAGE OverloadedStrings #-}
module Source.Rosie
  ( loadRosie
  ) where

import           Control.Arrow ((&&&))
import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import           Data.Foldable (fold)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP

import           Util
import           Document
import           Source.Solr
import           Source.DLTS

rosieRequest :: HTTP.Request
rosieRequest = HTTP.parseRequest_ "http://dev-dl-pa.home.nyu.edu/rosie/content.json"

parseRosie :: JSON.Object -> JSON.Parser Metadata
parseRosie o = do
  t <- mapM JSON.parseJSON $ HMap.delete "metadata" $ HMap.delete "representative_image" o
  m <- mapM pm =<< o JSON..: "metadata"
  return $ t <> m
  where
  pm v = fold . JSON.parseMaybe pv <$> JSON.withObject "rosie metadata" (JSON..: "value") v
  pv (JSON.Object v) = v JSON..: "value"
  pv v = JSON.parseJSON v

loadRosie :: HSet.HashSet T.Text -> IO Documents
loadRosie fl = do
  dd <- loadDLTS DLTSCore "rosie ss_handle:*" fl
  rj <- loadSolr rosieRequest "*" fl -- not really solr but acts like it?
  rm <- mapM (parseM parseRosie) $ V.toList rj
  let rh = HMap.fromList $ map (lv "handle" &&& id) rm
  mapM (\d@Document{ documentMetadata = dm } -> do
      let k = lv "ss_handle" dm
      m <- maybe (fail $ "missing rosie document: " ++ show k) return $ HMap.lookup k rh
      return d{ documentMetadata = dm <> m })
    dd
  where
  lv k = fold . foldMap values . HMap.lookup k
