{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cache
  ( updateCollection
  ) where

import           Control.Exception (SomeException, bracketOnError, try, throwIO)
#if MIN_VERSION_base(4,8,0)
import           Control.Exception (displayException)
#endif
import           Control.Monad (liftM2, guard, when)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Monoid (Any(..))
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime, addUTCTime)
import           System.FilePath ((<.>), splitFileName)
import           System.Directory (removeFile, renameFile)
import           System.IO (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), stderr, openBinaryFile, openTempFileWithDefaultPermissions, hFileSize, hSeek, hGetChar, hPutChar, hPutStr, hPutStrLn, hClose)

import           Util
import           Config
import           Document
import           Output.Primo

#if !MIN_VERSION_base(4,8,0)
displayException :: Exception e => e -> String
displayException = show
#endif

readBinaryFile :: FilePath -> (Handle -> IO a) -> IO a
readBinaryFile f h = bracketOnError
  (openBinaryFile f ReadMode)
  hClose
  h

_compareFiles :: FilePath -> FilePath -> IO Bool
_compareFiles f g = fromDoesNotExist False $ readBinaryFile f $ \h -> readBinaryFile g $ \i -> do
  x <- hFileSize h
  y <- hFileSize i
  if x == y
    then liftM2 (==)
      (BSLC.hGetContents h)
      (BSLC.hGetContents i)
    else return False

updateFile :: FilePath -> (Handle -> IO ()) -> IO ()
updateFile f w = bracketOnError
  (openTempFileWithDefaultPermissions fd ff)
  (\(tf, th) -> hClose th >> removeFile tf)
  (\(tf, th) -> do
    w th
    hClose th
    {- FIXME need two dates
    r <- _compareFiles tf f 
    if r
      then False <$ removeFile tf
      else True  <$ renameFile tf f -}
    renameFile tf f)
  where
  (fd, ff) = splitFileName f

-- |Run an operation, caching its result in a file, or retrieve the cached value if the file is newer than an interval.
-- If the operation throws an error, the error is cached in @file.err@, and any old cache is used for another time interval before retrying.
cache :: (JSON.ToJSON a, JSON.FromJSON a) => FilePath -> Maybe NominalDiffTime -> IO a -> IO a
cache f md g
  | Just d <- md = do
  new <- any . (<) . addUTCTime (negate d) <$> getCurrentTime
  m <- getModificationTime' f
  let load' = maybe id (const $ load . const) m -- load if cache exists
  if new m -- new enough?
    then load fail -- use cache
    else do
      me <- getModificationTime' fe
      if new me -- recent error?
        then load' $ fail =<< readFile fe -- use cache
        else update $ load' . throwIO -- reload
  | otherwise = update throwIO -- never use (but still update) cache
  where
  fe = f <.> "err" -- error file
  load e = either e return . JSON.eitherDecode =<< BSLC.readFile f
  update fall = try g >>= either
    (\e -> do -- write error file
      let s = displayException (e :: SomeException)
      hPutStrLn stderr $ f ++ ": " ++ s
      writeFile fe s
      fall e)
    (\x -> do -- success
      _ <- updateFile f $ \h -> BSLC.hPut h $ JSON.encode x
      fromDoesNotExist () $ removeFile fe
      return x)

readCollection :: Collection -> Bool -> IO Documents
readCollection c force =
  cache (collectionCache c) (collectionInterval c <$ guard (not force)) $ loadSource c

updateCollection :: Collection -> Bool -> UTCTime -> IO UTCTime
updateCollection c@Collection{ collectionCache = f } force t = do
  m <- getModificationTime0 f
  let uc = force || diffUTCTime t m >= collectionInterval c
  u <- case loadCollection c of
    Left l | uc -> -- meta-collection: load children
      getAny <$> foldMapM (\c' -> Any . (m <) <$> updateCollection c' force t) l
    _ -> return uc -- don't actually load a real collection
  if not u then return m else do
    when (collectionVerbose c) $ putStrLn $ "updating " ++ cis
    d <- either (return . Right . Left) (try . fmap Right) $ loadCollection c
    updateFile f $ \h -> either
      (\e -> do
        let s = cis ++ ": " ++ displayException (e :: SomeException)
        hPutStrLn stderr s
        hPutStr h s)
      (either
        (\l -> do
          mapM_ (\Collection{ collectionCache = cf } -> fromDoesNotExist () $
              readBinaryFile cf $ \r -> do
                z <- hFileSize r
                o <- if z > 2 then Just <$> hGetChar r else return Nothing
                if o == Just '['
                  then BSLC.hPut h . BSLC.cons ',' . BSLC.init =<< BSLC.hGetContents r
                  else hClose r)
            l
          hPutChar h ']'
          hSeek h AbsoluteSeek 0
          hPutChar h '[')
        (BSB.hPutBuilder h . outputPrimo))
      d
    getModificationTime0 f
  where
  cis = T.unpack $ collectionId c
