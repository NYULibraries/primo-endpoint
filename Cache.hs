{-# LANGUAGE RecordWildCards #-}
module Cache
  ( updateCollections
  ) where

import           Control.Exception (bracketOnError, handle, handleJust, SomeException)
import           Control.Monad (guard, when)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           System.FilePath ((</>), (<.>), splitFileName)
import           System.Directory (removeFile, renameFile, getModificationTime)
import           System.IO (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), stderr, openBinaryFile, openTempFileWithDefaultPermissions, hFileSize, hSeek, hPutChar, hPutStrLn, hClose)
import           System.IO.Error (isDoesNotExistError)

import           Document
import           Config

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

updateCollection :: Bool -> FilePath -> Collection -> IO (Maybe Documents)
updateCollection force f c = do
  r <- if force then return True else
    handleJust (guard . isDoesNotExistError) (\_ -> return True) $ do
      m <- getModificationTime f
      t <- getCurrentTime
      return $ diffUTCTime t m >= collectionInterval c
  d <- if r
    then handle
      (\e -> Nothing <$ hPutStrLn stderr (show (collectionSource c) ++ ": " ++ show (e :: SomeException)))
      $ Just <$> loadCollection c
    else return Nothing
  mapM_ (\j -> updateFile f $ \h -> BSLC.hPut h $ JSON.encode j) d
  return d

updateCollections :: Bool -> Config -> IO ()
updateCollections force Config{..} =
  updateFile (configCache </> "json") $ \h -> do
    mapM_ (\(k, c) -> do
        let f = configCache </> T.unpack k <.> "json"
        _ <- updateCollection force f c
        r <- openBinaryFile f ReadMode
        z <- hFileSize r
        when (z > 2) $ do
          hPutChar h ','
          BSLC.hPut h . BSLC.init . BSLC.tail =<< BSLC.hGetContents r)
      $ HM.toList configCollections
    hPutChar h ']'
    hSeek h AbsoluteSeek 0
    hPutChar h '['
