{-# LANGUAGE ViewPatterns #-}
module Source.JSON
  ( fileURI
  , loadJSON
  ) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTP (setUri)
import qualified Network.HTTP.Simple as HTTP
import           Network.URI (URI(..), URIAuth(..))

import           Document

fileURI :: FilePath -> URI
fileURI f = URI
  { uriScheme = "file:"
  , uriAuthority = Just URIAuth
    { uriUserInfo = ""
    , uriRegName = ""
    , uriPort = ""
    }
  , uriPath = f
  , uriQuery = ""
  , uriFragment = ""
  }

uriFile :: URI -> Maybe FilePath
uriFile URI
  { uriScheme = "file:"
  , uriAuthority = Just URIAuth
    { uriUserInfo = ""
    , uriRegName = ""
    , uriPort = ""
    }
  , uriPath = f
  , uriQuery = ""
  , uriFragment = ""
  } = Just f
uriFile _ = Nothing

loadJSON :: URI -> IO Documents
loadJSON (uriFile -> Just f) =
  either fail return . JSON.eitherDecode =<< BSLC.readFile f
loadJSON uri = do
  req <- HTTP.setUri HTTP.defaultRequest uri
  HTTP.responseBody <$> HTTP.httpJSON req
