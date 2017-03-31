{-# LANGUAGE OverloadedStrings #-}
module Output
  ( OutputFormat(..)
  , outputResponse
  ) where

import           Network.HTTP.Types (ok200, hContentType, Query)
import qualified Network.Wai as Wai

import           Config
import           Document
import           Output.HTML
import           Output.Primo
import           Output.MODS

data OutputFormat
  = OutputHTML
    { outputOrig :: Bool
    }
  | OutputPrimo
  | OutputMODS
  deriving (Eq, Show)

outputResponse :: OutputFormat -> Config -> Maybe Collection -> Query -> Documents -> Wai.Response
outputResponse (OutputHTML orig) conf coll q = Wai.responseBuilder ok200
  [ (hContentType, "text/html;charset=utf-8") ]
  . outputHTML conf coll q orig
outputResponse OutputPrimo _ _ _ = Wai.responseBuilder ok200
  [ (hContentType, "application/json") ]
  . outputPrimo
outputResponse OutputMODS _ _ _ = Wai.responseLBS ok200
  [ (hContentType, "application/xml") ]
  . outputMODS
