{-# LANGUAGE ScopedTypeVariables #-}
module ISO639
  ( ISO639
  , loadISO639
  , lookupISO639
  ) where

import           Control.Exception (SomeException, handle, handleJust)
import           Control.Monad (guard)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import           System.IO (withFile, IOMode(ReadMode))
import           System.IO.Error (isEOFError)

-- |Mapping from all ISO639 codes to the english name
type ISO639 = HMap.HashMap (CI.CI T.Text) T.Text

iso639Request :: HTTP.Request
iso639Request = HTTP.parseRequest_ "http://www.loc.gov/standards/iso639-2/ISO-639-2_utf-8.txt"

retrieveISO639 :: FilePath -> IO ()
retrieveISO639 f = BSLC.writeFile f . HTTP.responseBody =<< HTTP.httpLBS iso639Request 

readISO639 :: FilePath -> IO ISO639
readISO639 f = withFile f ReadMode $ loop HMap.empty where
  loop m h = handleJust (guard . isEOFError) (\_ -> return m) $ do
    [c3b, c3t, c2, eng, _] <- T.split ('|' ==) <$> T.hGetLine h
    let v = T.takeWhile (/= ';') eng
    loop (add c3t v $ add c3b v $ add c2 v m) h
  add c v
    | T.null c = id
    | otherwise = HMap.insertWith (flip T.append . T.cons '|') (CI.mk c) v

loadISO639 :: FilePath -> IO ISO639
loadISO639 f = handle (\(_ :: SomeException) -> retrieveISO639 f >> readISO639 f) $ readISO639 f

lookupISO639 :: ISO639 -> T.Text -> Maybe T.Text
lookupISO639 m c = HMap.lookup (CI.mk c) m
