{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module View
  ( view
  ) where

import           Control.Monad (join)
import qualified Data.ByteString.Char8 as BSC
import           Data.Function (on)
import qualified Data.HashMap.Strict as HMap
import           Data.Maybe (fromMaybe, isJust, isNothing)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.HTTP.Types.URI (Query)
import qualified Text.Blaze.Html5 as H
import           Text.Hamlet (hamlet, hamletFile)
import           Text.Read (readMaybe)

import           Config
import           Document

htmlDocument :: Document -> H.Html
htmlDocument m = H.dl
  $ HMap.foldrWithKey (\k v -> mappend $ H.dt (H.text k) <> H.dd (H.toMarkup v))
    mempty m

view :: Config -> Maybe Collection -> Documents -> Bool -> Query -> H.Html
view conf coll docs orig q =
  $(hamletFile "view.hamlet") nourls
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
