{-# LANGUAGE RecordWildCards #-}
module Cache
  ( updateCollections
  , getCollection
  ) where

import           Control.Arrow ((&&&))
import           Control.Exception (bracketOnError, try, SomeException)
import           Control.Monad (guard, liftM2)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HMap
import           Data.Monoid (Any(Any))
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime(..), diffUTCTime)
import           System.FilePath (splitFileName)
import           System.Directory (removeFile, renameFile)
import           System.IO (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), stderr, openBinaryFile, openTempFileWithDefaultPermissions, hFileSize, hSeek, hGetChar, hPutChar, hPutStr, hPutStrLn, hClose)

import           Util
import           Config

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
  r <- if force then return Nothing else do
    m <- getModificationTime0 f
    return $ m <$ guard (diffUTCTime t m < collectionInterval c)
  maybe
    (do
      d <- try $ loadCollection c
      _ <- updateFile f $ \h -> either
        (\e -> do
          let s = show (collectionSource c) ++ ": " ++ show (e :: SomeException)
          hPutStrLn stderr s
          hPutStr h s)
        (BSLC.hPut h . JSON.encode)
        d
      getModificationTime0 f)
    return
    r

updateCollections :: Config -> Bool -> UTCTime -> IO UTCTime
updateCollections Config{ configCache = f, configCollections = l } force t = do
  m <- getModificationTime0 f
  Any u <- foldMapM (\c -> Any . (m <) <$> updateCollection c force t) l
  r <- if u
    then
      updateFile f $ \h -> do
        mapM_ (\c -> fromDoesNotExist () $
            readBinaryFile (collectionCache c) $ \r -> do
              z <- hFileSize r
              o <- if z > 2 then Just <$> hGetChar r else return Nothing
              if o == Just '['
                then BSLC.hPut h . BSLC.cons ',' . BSLC.init =<< BSLC.hGetContents r
                else hClose r)
          l
        hPutChar h ']'
        hSeek h AbsoluteSeek 0
        hPutChar h '['
    else return False
  if r then getModificationTime0 f else return m

getCollection :: Config -> Maybe T.Text -> Maybe (FilePath, Bool -> UTCTime -> IO UTCTime)
getCollection conf Nothing = Just (configCache conf, updateCollections conf)
getCollection conf (Just c) = (collectionCache &&& updateCollection) <$> HMap.lookup c (configCollections conf)
