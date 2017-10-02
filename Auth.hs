{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Auth
  ( loadAuth
  , applyAuth
  ) where

import           Control.Applicative ((<|>))
import           Control.Arrow (first, (***))
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HMap
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as YAML
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Header (RequestHeaders)
import           Data.List (nubBy)
import           Data.Function (on)
import           System.Directory (doesFileExist)
import           Debug.Trace
import           Util

data Auth = Auth
  { authUser :: Maybe BS.ByteString
  , authPass :: Maybe BS.ByteString
  , authHeaders :: RequestHeaders
  }

instance Monoid Auth where
  mempty = Auth Nothing Nothing []
  mappend (Auth u1 p1 h1) (Auth u2 p2 h2) =
    Auth (u1 <|> u2) (p1 <|> p2) (h1 ++ h2)

tBS :: T.Text -> BS.ByteString
tBS = TE.encodeUtf8

instance JSON.FromJSON Auth where
  parseJSON = JSON.withObject "auth" $ \o -> Auth
    <$> (fmap tBS <$> o JSON..:? "user")
    <*> (fmap tBS <$> o JSON..:? "pass")
    <*> (maybe [] (map (CI.mk . tBS *** tBS) . HMap.toList) <$> o JSON..:? "headers")

type AuthSettings = HMap.HashMap BS.ByteString Auth

loadAuth :: FilePath -> IO AuthSettings
loadAuth f = do
  e <- doesFileExist f
  if e
    then HMap.fromList . map (first tBS) . HMap.toList <$> (parseJSONM . fromMaybe JSON.emptyObject =<< YAML.decodeFile f)
    else return HMap.empty

applyAuth :: AuthSettings -> HTTP.ManagerSettings -> HTTP.ManagerSettings
applyAuth auths hm = hm
  { HTTP.managerModifyRequest = \req -> do
    req' <- HTTP.managerModifyRequest hm req
    return $ maybe req' (\Auth{..} -> 
      fromMaybe id (HTTP.applyBasicAuth <$> authUser <*> authPass)
        req'{ HTTP.requestHeaders = traceShowId $ nubBy ( (==) `on` fst) $ authHeaders ++ HTTP.requestHeaders req' })
      $ HMap.lookup (HTTP.host req') auths
  }
