{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Output.Primo
  ( outputPrimo
  ) where

import qualified Data.Aeson.Encoding as JSONE
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Builder as BSB
import qualified Data.HashMap.Strict as HMap
import           Data.Monoid ((<>))
import qualified Data.Vector as V

import           Document

-- |Ichabod-solr-compatible JSON output format for Primo
outputPrimo :: Documents -> BSB.Builder
outputPrimo = JSON.fromEncoding . JSONE.list
  (JSON.pairs . HMap.foldrWithKey (\k v a -> f k v <> a) mempty) . V.toList where
  f "id" = keyValueJSON' "id"
  f "collection" = keyValueJSON' "collection_ssm"
  f k = keyValueJSON ("desc_metadata__" <> k <> "_tesim")
