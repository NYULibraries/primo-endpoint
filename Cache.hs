{-# LANGUAGE RecordWildCards #-}
module Cache
  ( updateCollection
  , updateCollections
  ) where

import           Control.Exception (bracketOnError, handle, handleJust, SomeException)
import           Control.Monad (guard, liftM2)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Monoid (Any(Any))
import           Data.Time.Clock (UTCTime(..), diffUTCTime)
import           System.FilePath (splitFileName)
import           System.Directory (removeFile, renameFile, getModificationTime)
import           System.IO (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), stderr, openBinaryFile, openTempFileWithDefaultPermissions, hFileSize, hSeek, hPutChar, hPutStrLn, hClose)
import           System.IO.Error (isDoesNotExistError)

import           Util
import           Config

fromDoesNotExist :: a -> IO a -> IO a
fromDoesNotExist d = handleJust (guard . isDoesNotExistError) (\_ -> return d)

getModificationTime0 :: FilePath -> IO UTCTime
getModificationTime0 = fromDoesNotExist (UTCTime (toEnum 0) 0) . getModificationTime

readBinaryFile :: FilePath -> (Handle -> IO a) -> IO a
readBinaryFile f h = bracketOnError
  (openBinaryFile f ReadMode)
  hClose
  h

compareFiles :: FilePath -> FilePath -> IO Bool
compareFiles f g = fromDoesNotExist False $ readBinaryFile f $ \h -> readBinaryFile g $ \i -> do
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
    let r = False -- r <- compareFiles tf f -- FIXME need two dates
    if r
      then False <$ removeFile tf
      else True  <$ renameFile tf f)
  where
  (fd, ff) = splitFileName f

updateCollection :: Bool -> Collection -> UTCTime -> IO UTCTime
updateCollection force c@Collection{ collectionCache = f } t = do
  r <- if force then return Nothing else do
    m <- getModificationTime0 f
    return $ m <$ guard (diffUTCTime t m < collectionInterval c)
  maybe
    (do
      d <- handle
        (\e -> Nothing <$ hPutStrLn stderr (show (collectionSource c) ++ ": " ++ show (e :: SomeException)))
        $ Just <$> loadCollection c
      mapM_ (\j -> updateFile f $ \h -> BSLC.hPut h $ JSON.encode j) d
      getModificationTime0 f)
    return
    r

updateCollections :: Bool -> Config -> UTCTime -> IO UTCTime
updateCollections force Config{ configCache = f, configCollections = l } t = do
  m <- getModificationTime0 f
  Any u <- foldMapM (\c -> Any . (m <) <$> updateCollection force c t) l
  r <- if u
    then
      updateFile f $ \h -> do
        mapM_ (\c -> fromDoesNotExist () $
            readBinaryFile (collectionCache c) $ \r -> do
              z <- hFileSize r
              if z > 2
                then do
                  hPutChar h ','
                  BSLC.hPut h . BSLC.init . BSLC.tail =<< BSLC.hGetContents r
                else
                  hClose h)
          l
        hPutChar h ']'
        hSeek h AbsoluteSeek 0
        hPutChar h '['
    else return False
  if r then getModificationTime0 f else return m
