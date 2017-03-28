{-# LANGUAGE OverloadedStrings #-}
module Source.SDR
  ( loadSDR
  ) where

import           Data.Monoid ((<>))
import qualified Network.HTTP.Client as HTTP

import           Util
import           Document
import           Source.Blacklight

sdrRequest :: HTTP.Request
sdrRequest = HTTP.parseRequest_ "https://geo.nyu.edu/catalog"

parseSDR :: Monad m => Metadata -> m Document
parseSDR m = do
  i <- handleToID =<< oneValue (getMetadata m "dc_identifier_s")
  return $ mkDocument
    ("sdr:" <> i)
    "Spatial Data Repository"
    m

loadSDR :: IO Documents
loadSDR =
  parseM (mapM parseSDR)
    =<< loadBlacklight sdrRequest [("dct_provenance_s", "NYU")] "layer_id_s"
