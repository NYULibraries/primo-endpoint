{-# LANGUAGE OverloadedStrings #-}
module Source.ISAW
  ( loadISAW
  ) where

import           Control.Monad (void)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit.Attoparsec (sinkParser)
import qualified Data.HashMap.Strict as HMap
import qualified Network.HTTP.Simple as HTTP
import qualified Codec.Archive.Zip.Conduit.UnZip as Z

import           Document

isawRequest :: HTTP.Request
isawRequest = HTTP.parseRequest_ "http://isaw.nyu.edu/publications/awol-index/awol-index-json.zip"

parseISAW :: JSON.Value -> JSON.Parser (Maybe Document)
parseISAW = JSON.withObject "isaw json" $ \o -> return $
  case HMap.lookupDefault JSON.Null "is_part_of" o of
    JSON.Null -> Just $ parseNestedMetadata "." "" $ JSON.Object $ HMap.delete "provenance" o
    _ -> Nothing

whileRight :: Monad m => C.Conduit (Either a b) m b
whileRight = C.await >>= maybe
  (return ())
  (either (C.leftover . Left) (\b -> C.yield b >> whileRight))

streamISAW :: C.Conduit (Either Z.ZipEntry BS.ByteString) IO Document
streamISAW = mapM_ (\e -> entry e >> streamISAW) =<< C.await where
  entry (Left Z.ZipEntry{ Z.zipEntryName = n })
    | ".json" `BS.isSuffixOf` n =
      mapM_ C.yield
        =<< either (fail . ((BSC.unpack n ++ ": ") ++)) return . JSON.parseEither parseISAW
        =<< whileRight C..| sinkParser JSON.json'
    | otherwise = return ()
  entry (Right _) = fail "Unexpected file data"

loadISAW :: IO Documents
loadISAW = HTTP.httpSink isawRequest $ \_ ->
  void Z.unZipStream C..| streamISAW C..| CC.sinkVector
