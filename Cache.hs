{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cache
  ( defaultCacheDir
  , cache
  ) where

import           Control.Exception (SomeException, bracketOnError, try, throwIO)
#if MIN_VERSION_base(4,8,0)
import           Control.Exception (displayException)
#endif
import           Control.Monad (liftM2)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Time.Clock (UTCTime)
import           System.FilePath ((<.>), splitFileName)
import           System.Directory (removeFile, renameFile
#if MIN_VERSION_directory(1,2,3)
  , getXdgDirectory, XdgDirectory(XdgCache)
#else
  , getHomeDirectory
#endif
  )
import           System.IO (Handle, IOMode(ReadMode), stderr, openBinaryFile, openTempFileWithDefaultPermissions, hFileSize, hPutStrLn, hClose)

import           Util

defaultCacheDir :: IO FilePath
defaultCacheDir =
#if MIN_VERSION_directory(1,2,3)
  (getXdgDirectory XdgCache "primo-endpoint")
#else
  ((</> ".cache" </> "primo-endpoint") <$> getHomeDirectory)
#endif

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

-- |Run an operation, caching its result in a file, or retrieve the cached value if the file is newer than a time.
-- If the operation throws an error, the error is cached in @file.err@, and any old cache is used until the error also ages out.
cache :: (JSON.ToJSON a, JSON.FromJSON a) => FilePath -> Maybe UTCTime -> IO a -> IO a
cache f mt g
  | Just t <- mt = do
  m <- getModificationTime' f
  let load' = maybe id (const $ load . const) m -- load if cache exists
  if any (t <) m -- new enough?
    then load fail -- use cache
    else do
      me <- getModificationTime' fe
      if any (t <) me -- recent error?
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
