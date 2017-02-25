{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Exception (throwIO)
import           Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import qualified Data.Yaml as YAML
import           Network.Connection (TLSSettings(..))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified System.Console.GetOpt as Opt
import           System.Directory (createDirectoryIfMissing
#if MIN_VERSION_directory(1,2,3)
  , getXdgDirectory, XdgDirectory(XdgCache)
#else
  , getHomeDirectory
#endif
  )
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.FilePath ((</>), takeDirectory)
import           System.IO (hPutStrLn, stderr)

import           Config
import           Cache
import           Server

data Opts = Opts
  { optConfig :: FilePath
  , optCache :: Maybe FilePath
  , optForce :: Bool
  , optOutput :: Maybe String
  , optServer :: Maybe Int
  }

defOpts :: Opts
defOpts = Opts
  { optConfig = "config.yml"
  , optCache = Nothing
  , optForce = False
  , optOutput = Nothing
  , optServer = Nothing
  }

opts :: [Opt.OptDescr (Opts -> Opts)]
opts =
  [ Opt.Option "c" ["config"] (Opt.ReqArg (\f o -> o{ optConfig = f }) "FILE")
    ("Load configuration from FILE [" ++ optConfig defOpts ++ "]")
  , Opt.Option "C" ["cache"] (Opt.ReqArg (\f o -> o{ optCache = Just f }) "DIR")
    "Use DIR for cache files [$XDR_CACHE_DIR/primo-endpoint]"
  , Opt.Option "f" ["force"] (Opt.NoArg (\o -> o{ optForce = True }))
    "Force an initial update of all collections"
  , Opt.Option "o" ["output"] (Opt.OptArg (\f o -> o{ optOutput = Just (fromMaybe "-" f) }) "DEST")
    "Write JSON output to file [-]"
  , Opt.Option "w" ["web-server"] (Opt.OptArg (\f o -> o{ optServer = Just (maybe 80 read f) }) "PORT")
    "Run a web server on PORT [80] to serve the result"
  ]

outputFile :: String -> BSLC.ByteString -> IO ()
outputFile "-" = BSLC.putStr
outputFile f = BSLC.writeFile f

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  Opts{..} <- case Opt.getOpt Opt.Permute opts args of
    (ol, [], []) -> return $ foldl' (flip ($)) defOpts ol
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]") opts
      exitFailure
  
  cache <- maybe 
#if MIN_VERSION_directory(1,2,3)
    (getXdgDirectory XdgCache "primo-endpoint")
#else
    ((</> ".cache" </> "primo-endpoint") <$> getHomeDirectory)
#endif
    return optCache
  config <- either throwIO (return . setCacheDir cache) =<< YAML.decodeFileEither optConfig

  HTTPS.setGlobalManager =<< HTTP.newManager (HTTPS.mkManagerSettings (TLSSettingsSimple True False False) Nothing)

  createDirectoryIfMissing False $ takeDirectory $ configCache config
  idx <- loadIndices config
  updateCollections idx optForce config

  mapM_ (\o -> outputFile o =<< BSLC.readFile (configCache config)) optOutput

  forM_ optServer $ \port -> do
    server port (configCache config)
