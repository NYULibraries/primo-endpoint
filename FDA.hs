{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module FDA
  ( readFDA
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import           Data.Char (isDigit)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V

import           Document

oneOrMany :: (JSON.Value -> JSON.Parser a) -> JSON.Value -> JSON.Parser [a]
oneOrMany p (JSON.Array l) = mapM p $ V.toList l
oneOrMany p a = return <$> p a

readFDA :: JSON.Value -> JSON.Parser [Document]
readFDA = oneOrMany $ JSON.withObject "FDA" $ \obj -> do
  (handle0, handle1) <- readHandle =<< obj JSON..: "handle"
  name <- obj JSON..: "name"
  metadata <- readMetadata =<< obj JSON..: "metadata"
  return $ Document
    { documentID = "fda:hdl-handle-net-" <> handle0 <> "-" <> handle1
    , documentCollection = name
    , documentMetadata = metadata
    }
  where
  readHandle (T.break ('/' ==) -> (a@(T.all isDigit -> True), T.uncons -> Just ('/', b@(T.all isDigit -> True)))) = return (a, b)
  readHandle s = fail $ "invalid FDA handle: " ++ show s
  readField :: JSON.Value -> JSON.Parser (T.Text, T.Text)
  readField = JSON.withObject "FDA.metadata.field" $ \o -> do
    (T.stripPrefix "dc." -> Just key) <- o JSON..: "key"
    value <- o JSON..: "value"
    return (key, value)
  readMetadata = JSON.withArray "FDA.metadata" $
    V.foldM (\m f -> uncurry (addMetadata m) <$> readField f) mempty
