{-# LANGUAGE OverloadedStrings #-}
module Source.Rosie
  ( loadRosie
  ) where

import           Control.Arrow ((&&&))
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V

import           Document
import           Source.DLTS
import           Source.DLib

loadRosie :: HSet.HashSet T.Text -> IO Documents
loadRosie fl = do
  dd <- loadDLTS DLTSCore "rosie ss_handle:*" fl
  rd <- loadDLib "rosie" "" "rosie/content.json"
  let rm = HMap.fromList $ map (documentID &&& documentMetadata) $ V.toList rd
  mapM (\d@Document{ documentID = i } -> do
      m <- maybe (fail $ "missing rosie document: " ++ show i) return $ HMap.lookup i rm
      return d{ documentMetadata = documentMetadata d <> m })
    dd
