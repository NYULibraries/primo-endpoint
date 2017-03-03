module Paginate
  ( joinPages
  ) where

import qualified Data.Vector as V

pageLength :: Int
pageLength = 1000

joinPages :: Monad m => (Int -> Int -> m (V.Vector a, Int)) -> m (V.Vector a)
joinPages f = V.concat <$> loop 0 pageLength where
  loop _ 0 = return []
  loop start rows = do
    (r, n) <- f start rows
    let s = start + V.length r
    if V.length r == 0
      then return []
      else (r :) <$> loop s (min pageLength $ n - s)
