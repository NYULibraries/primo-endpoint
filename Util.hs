module Util
  ( foldMapM
  , jsonOr
  , withObjectOrNull
  , withArrayOrNull
  , withArrayOrSingleton
  , withArrayOrNullOrSingleton
  ) where

import qualified Data.Aeson.Types as JSON
import           Data.Foldable (foldlM)
import qualified Data.Vector as V

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM (\b a -> mappend b <$> f a) mempty

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
