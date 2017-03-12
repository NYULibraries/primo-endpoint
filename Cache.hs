{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cache
  ( updateCollection
  ) where

import           Control.Exception (bracketOnError, try, SomeException)
#if MIN_VERSION_base(4,8,0)
import           Control.Exception (displayException)
#endif
import           Control.Monad (liftM2, when)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Monoid (Any(..))
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime(..), diffUTCTime)
import           System.FilePath (splitFileName)
import           System.Directory (removeFile, renameFile)
import           System.IO (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), stderr, openBinaryFile, openTempFileWithDefaultPermissions, hFileSize, hSeek, hGetChar, hPutChar, hPutStr, hPutStrLn, hClose)

import           Util
import           Config

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

updateFile :: FilePath -> (Handle -> IO ()) -> IO Bool
updateFile f w = bracketOnError
  (openTempFileWithDefaultPermissions fd ff)
  (\(tf, th) -> hClose th >> removeFile tf)
  (\(tf, th) -> do
    w th
    hClose th
    let r = False -- r <- _compareFiles tf f -- FIXME need two dates
    if r
      then False <$ removeFile tf
      else True  <$ renameFile tf f)
  where
  (fd, ff) = splitFileName f

updateCollection :: Collection -> Bool -> UTCTime -> IO UTCTime
updateCollection c@Collection{ collectionCache = f } force t = do
  m <- getModificationTime0 f
  let uc = force || diffUTCTime t m >= collectionInterval c
  u <- case loadCollection c of
    Left l | uc ->
      getAny <$> foldMapM (\c' -> Any . (m <) <$> updateCollection c' force t) l
    _ -> return uc
  if not u then return m else do
    when (collectionVerbose c) $ putStrLn $ "updating " ++ cis
    d <- either (return . Right . Left) (try . fmap Right) $ loadCollection c
    r <- updateFile f $ \h -> either
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
        (BSLC.hPut h . JSON.encode))
      d
    if r then getModificationTime0 f else return m
  where
  cis = T.unpack $ collectionId c
