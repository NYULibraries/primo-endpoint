{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Source.SpecialCollections
  ( loadSpecialCollections
  ) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP

import           Util
import           Document
import           Source.Blacklight

scRequest :: HTTP.Request
scRequest = HTTP.parseRequest_ "https://specialcollections.library.nyu.edu/search/catalog.json"

parseSC :: Monad m => T.Text -> Metadata -> m Document
parseSC key m = do
  r <- splid <$> oneValue (getMetadata m "ref_ssi")
  c <- oneValue (getMetadata m "collection_ssm")
  return Document
    { documentID = key <> T.cons ':' (key <> r)
    , documentCollection = c
    , documentMetadata =
        HMap.insert "_ref_ssi" (value r)
      $ HMap.insert "_parent_ssm" (mapValues splid $ getMetadata m "parent_ssm")
      $ m
    }
  where
  -- woj items have ids like "wojaspace_ref13" and we want "ref13"
  -- no idea if this generalizes across collections
  splid i = case T.break ('_' ==) i of
    (_, T.uncons -> Just ('_', r)) -> r
    _ -> i

loadSpecialCollections :: T.Text -> [(BS.ByteString, BS.ByteString)] -> IO Documents
loadSpecialCollections key fq =
  parseM (mapM $ parseSC key)
    =<< loadBlacklight scRequest fq "id"
