{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Output.HTML
  ( outputHTML
  ) where

import           Control.Monad (join)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import           Data.Function (on)
import qualified Data.HashMap.Strict as HMap
import           Data.List (sortOn)
import           Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.HTTP.Types.URI (Query)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import           Text.Hamlet (hamlet, hamletFile)
import           Text.Read (readMaybe)

import           Config
import           Document

outputHTML :: Config -> Maybe Collection -> Query -> Bool -> Documents -> BSB.Builder
outputHTML conf coll q orig docs = renderHtmlBuilder
  $ $(hamletFile "Output/HTML.hamlet") nourls
  where
  collName c = fromMaybe (collectionKey c) (collectionName c)
  n = V.length docs
  np = (n + count - 1) `div` count
  l = V.take count $ V.drop (count * pred page) docs
  getq b k = fromMaybe b $ readMaybe . BSC.unpack =<< join (lookup k q)
  page = max 1 $ min np $ getq (getq 1 "page") "nav"
  count = max 1 $ getq 10 "count"
  navto (i :: Int) = [hamlet|<input type=submit name=nav value=#{i}>|]
  nourls () _ = T.empty
