{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Collection
  ( loadCollection
  , generateCollection
  ) where

import qualified Data.HashMap.Strict as HMap
import           Data.Time.Clock (UTCTime, addUTCTime)
import qualified Data.Vector as V

import           Util
import           Cache
import           Config
import           Document
import           Fields

loadCollection :: Config -> Maybe UTCTime -> Collection -> IO Documents
loadCollection conf t c =
  cache (collectionCache c) (addUTCTime . negate <$> collectionInterval c <*> t) $ loadSource conf c

generateCollection :: Config -> Maybe UTCTime -> Maybe Collection -> IO Documents
generateCollection conf t (Just c) = V.map
  (generateFields (collectionFields c)
    . HMap.insert "_key" (value $ collectionKey c)
    . HMap.insert "_name" (foldMap value $ collectionName c))
  <$> loadCollection conf t c
generateCollection conf t Nothing =
  foldMapM (generateCollection conf t . Just) $ configCollections conf
