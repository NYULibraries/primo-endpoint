{-# LANGUAGE OverloadedStrings #-}
module Source.SDR
  ( loadSDR
  ) where

import qualified Network.HTTP.Client as HTTP

import           Document
import           Source.Blacklight

sdrRequest :: HTTP.Request
sdrRequest = HTTP.parseRequest_ "https://geo.nyu.edu/catalog"

loadSDR :: IO Documents
loadSDR = loadBlacklight sdrRequest [("dct_provenance_s", "NYU")] "layer_id_s"
