{-# LANGUAGE RecordWildCards #-}
module Cache
  ( updateCollections
  ) where

import           Control.Exception (bracketOnError, handle, handleJust, SomeException)
import           Control.Monad (guard, when)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Maybe (isJust)
import           Data.Monoid (Any(Any))
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           System.FilePath (splitFileName)
import           System.Directory (removeFile, renameFile, getModificationTime)
import           System.IO (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), stderr, openBinaryFile, openTempFileWithDefaultPermissions, hFileSize, hSeek, hPutChar, hPutStrLn, hClose)
import           System.IO.Error (isDoesNotExistError)

import           Util
import           Document
import           Config

fromDoesNotExist :: a -> IO a -> IO a
fromDoesNotExist d = handleJust (guard . isDoesNotExistError) (\_ -> return d)

updateFile :: FilePath -> (Handle -> IO ()) -> IO ()
updateFile f w = bracketOnError
  (openTempFileWithDefaultPermissions fd ff)
  (removeFile . fst)
  (\(tf, th) -> do
    w th
    hClose th
    renameFile tf f)
  where
  (fd, ff) = splitFileName f

updateCollection :: Bool -> Collection -> IO (Maybe Documents)
updateCollection force c = do
  r <- if force then return True else
    fromDoesNotExist True $ do
      m <- getModificationTime $ collectionCache c
      t <- getCurrentTime
      return $ diffUTCTime t m >= collectionInterval c
  d <- if r
    then handle
      (\e -> Nothing <$ hPutStrLn stderr (show (collectionSource c) ++ ": " ++ show (e :: SomeException)))
      $ Just <$> loadCollection c
    else return Nothing
  mapM_ (\j -> updateFile (collectionCache c) $ \h -> BSLC.hPut h $ JSON.encode j) d
  return d

updateCollections :: Bool -> Config -> IO ()
updateCollections force Config{..} = do
  Any u <- foldMapM (fmap (Any . isJust) . updateCollection force) configCollections
  when u $
    updateFile configCache $ \h -> do
      mapM_ (\c -> fromDoesNotExist () $ do
          r <- openBinaryFile (collectionCache c) ReadMode
          z <- hFileSize r
          if z > 2
            then do
              hPutChar h ','
              BSLC.hPut h . BSLC.init . BSLC.tail =<< BSLC.hGetContents r
            else
              hClose r)
        configCollections
      hPutChar h ']'
      hSeek h AbsoluteSeek 0
      hPutChar h '['
