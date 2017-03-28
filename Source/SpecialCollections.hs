{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Source.SpecialCollections
  ( loadSpecialCollections
  ) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP

import           Util
import           Document
import           Source.Blacklight

scRequest :: HTTP.Request
scRequest = HTTP.parseRequest_ "https://specialcollections.library.nyu.edu/search/catalog.json"

parseSC :: Monad m => Metadata -> m Document
parseSC m = do
  r <- splid <$> oneValue (getMetadata m "ref_ssi")
  return
    $ HMap.insert "_ref_ssi" (value r)
    $ HMap.insert "_parent_ssm" (mapValues splid $ getMetadata m "parent_ssm")
    m
  where
  -- woj items have ids like "wojaspace_ref13" and we want "ref13"
  -- no idea if this generalizes across collections
  splid i = case T.break ('_' ==) i of
    (_, T.uncons -> Just ('_', r)) -> r
    _ -> i

loadSpecialCollections :: [(BS.ByteString, BS.ByteString)] -> IO Documents
loadSpecialCollections fq =
  parseM (mapM parseSC)
    =<< loadBlacklight scRequest fq "id"
