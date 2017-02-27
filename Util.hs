module Util
  ( foldMapM
  , fromDoesNotExist
  , getModificationTime0
  , jsonOr
  , withObjectOrNull
  , withArrayOrNull
  , withArrayOrSingleton
  , withArrayOrNullOrSingleton
  , parseM
  , parseJSONM
  , addRequestPath
  ) where

import           Control.Exception (handleJust)
import           Control.Monad (guard)
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable (foldlM)
import           Data.Monoid ((<>))
import           Data.Time.Clock (UTCTime(..))
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import           System.Directory (getModificationTime)
import           System.IO.Error (isDoesNotExistError)

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM (\b a -> mappend b <$> f a) mempty

fromDoesNotExist :: a -> IO a -> IO a
fromDoesNotExist d = handleJust (guard . isDoesNotExistError) (\_ -> return d)

getModificationTime0 :: FilePath -> IO UTCTime
getModificationTime0 = fromDoesNotExist (UTCTime (toEnum 0) 0) . getModificationTime

jsonOr :: JSON.Value -> JSON.Value -> JSON.Value
jsonOr JSON.Null a = a
jsonOr a _ = a

withObjectOrNull :: String -> (JSON.Object -> JSON.Parser a) -> JSON.Value -> JSON.Parser a
withObjectOrNull s f o = JSON.withObject s f $ jsonOr o JSON.emptyObject

withArrayOrNull :: String -> (JSON.Array -> JSON.Parser a) -> JSON.Value -> JSON.Parser a
withArrayOrNull s f o = JSON.withArray s f $ jsonOr o JSON.emptyArray

withArrayOrSingleton :: (JSON.Array -> JSON.Parser a) -> JSON.Value -> JSON.Parser a
withArrayOrSingleton f (JSON.Array a) = f a
withArrayOrSingleton f v = f $ V.singleton v

withArrayOrNullOrSingleton :: (JSON.Array -> JSON.Parser a) -> JSON.Value -> JSON.Parser a
withArrayOrNullOrSingleton f (JSON.Array a) = f a
withArrayOrNullOrSingleton f JSON.Null = f V.empty
withArrayOrNullOrSingleton f v = f $ V.singleton v

parseM :: Monad m => (a -> JSON.Parser b) -> a -> m b
parseM f = either fail return . JSON.parseEither f

parseJSONM :: (Monad m, JSON.FromJSON a) => JSON.Value -> m a
parseJSONM = parseM JSON.parseJSON

addRequestPath :: HTTP.Request -> BSC.ByteString -> HTTP.Request
addRequestPath q p = q{ HTTP.path = HTTP.path q <> BSC.cons '/' p }
