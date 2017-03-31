{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Output.MODS
  ( outputMODS
  ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.ICU as TU
import           Network.URI (parseURI)
import           Text.Hamlet.XML (xml, xmlFile)
import qualified Text.XML as XML

import           Document

-- |Test if a regular expression matches the text
matches :: T.Text -> T.Text -> Bool
matches p = isJust . TU.find (TU.regex [] p)

likeDate :: T.Text -> Bool
likeDate = matches "[0-9][0-9?][0-9?]" -- contains at least 3 "digits"

-- You may ask, why are we first pasting together publisher fields and then splitting them apart again.
-- That's a good question, but presumably some sources may already have joined publishers...
splitPublisher :: T.Text -> Maybe (T.Text, T.Text, T.Text)
splitPublisher s = case T.breakOn ":" s of
  (_, "") -> Nothing
  (p, T.tail -> r) -> case T.breakOnEnd "," r of
    ("", _) -> Nothing
    (T.init -> a, d) | likeDate d -> Just (a, p, d)
    _ -> Nothing

splitName :: T.Text -> (T.Text, Maybe T.Text, Maybe T.Text)
splitName s = case reverse $ T.splitOn delim s of
  a : b : r | likeRole a, likeDate b -> (rest r, Just b, Just a)
  a     : r |             likeDate a -> (rest r, Just a, Nothing)
  a     : r | likeRole a             -> (rest r, Nothing, Just a)
  _ -> (s, Nothing, Nothing)
  where
  likeRole = matches "^ *[a-z]" -- starts with lower-case letter
  rest = T.intercalate delim . reverse
  delim = ", "

documentMODS :: Document -> XML.Document
documentMODS doc = XML.Document
  (XML.Prologue [] Nothing [])
  (case $(xmlFile "Output/MODS.xml") of { ~[XML.NodeElement e] -> e })
  []
  where
  get = values . getMetadata doc
  identType i
    | T.isPrefixOf "http://hdl.handle.net/" i = "hdl"
    | T.isPrefixOf "http://dx.doi.org/" i = "doi"
    | T.isPrefixOf "http://doi.org/" i = "doi"
    | Just _ <- parseURI (T.unpack i) = "uri"
    | otherwise = "local" -- ???
  nameXML dr (splitName -> (n, nd, nr)) = [xml|
      <role>
        <roleTerm type="text">#{fromMaybe dr nr}
      <namePart>#{n}
      $maybe d <- nd
        <namePart type="date">#{d}
    |]

outputMODS :: Documents -> BSL.ByteString
outputMODS = foldMap $ XML.renderLBS XML.def . documentMODS
