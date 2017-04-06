{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Monad (forM_)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.List (foldl')
import qualified Data.HashMap.Strict as HMap
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Time.Clock (getCurrentTime)
import           Network.Connection (TLSSettings(..))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified System.Console.GetOpt as Opt
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (Handle, hPutStrLn, stdout, stderr, withFile, IOMode(WriteMode))

import           Cache
import           Config
import           Auth
import           Collection
import           Output.Primo
import           Output.MODS
import           Server

data Opts = Opts
  { optConfig :: FilePath
  , optAuth :: FilePath
  , optCache :: Maybe FilePath
  , optForce :: Bool
  , optCollection :: Maybe String
  , optOutput :: Maybe String
  , optMODS :: Bool
  , optServer :: Maybe Int
  , optLog :: Bool
  , optVerbose :: Bool
  }

defOpts :: Opts
defOpts = Opts
  { optConfig = "config.yml"
  , optAuth = "auth.yml"
  , optCache = Nothing
  , optForce = False
  , optCollection = Nothing
  , optOutput = Nothing
  , optMODS = False
  , optServer = Nothing
  , optLog = False
  , optVerbose = False
  }

opts :: [Opt.OptDescr (Opts -> Opts)]
opts =
  [ Opt.Option "c" ["config"] (Opt.ReqArg (\f o -> o{ optConfig = f }) "FILE")
    ("Load configuration from FILE [" ++ optConfig defOpts ++ "]")
  , Opt.Option "a" ["auth"] (Opt.ReqArg (\f o -> o{ optAuth = f }) "FILE")
    ("Load auth rules from FILE [" ++ optAuth defOpts ++ "]")
  , Opt.Option "C" ["cache"] (Opt.ReqArg (\f o -> o{ optCache = Just f }) "DIR")
    "Use DIR for cache files [$XDR_CACHE_DIR/primo-endpoint]"
  , Opt.Option "f" ["force"] (Opt.NoArg (\o -> o{ optForce = True }))
    "Force an initial update of collections"
  , Opt.Option "o" ["output"] (Opt.OptArg (\f o -> o{ optOutput = Just (fromMaybe "-" f) }) "DEST")
    "Write output to file [-]"
  , Opt.Option "k" ["collection"] (Opt.ReqArg (\f o -> o{ optCollection = Just f }) "KEY")
    "Limit -o and -f to a single collection"
  , Opt.Option "x" ["MODS"] (Opt.NoArg (\o -> o{ optMODS = True }))
    "Output (-o) in MODS XML format [Primo JSON]"
  , Opt.Option "w" ["web-server"] (Opt.OptArg (\f o -> o{ optServer = Just (maybe 80 read f) }) "PORT")
    "Run a web server on PORT [80] to serve the result"
  , Opt.Option "l" ["log-access"] (Opt.NoArg (\o -> o{ optLog = True }))
    "Log access to stdout"
  , Opt.Option "v" ["verbose"] (Opt.NoArg (\o -> o{ optVerbose = True }))
    "Log collection refreshes to stdout"
  ]

outputFile :: (Handle -> IO ()) -> String -> IO ()
outputFile w "-" = w stdout
outputFile w f = withFile f WriteMode w

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
  
  auth <- loadAuth optAuth
  HTTPS.setGlobalManager =<< HTTP.newManager (applyAuth auth $ HTTPS.mkManagerSettings (TLSSettingsSimple True False False) Nothing)

  cdir <- maybe
    defaultCacheDir
    return optCache
  createDirectoryIfMissing True cdir
  config <- loadConfig optForce cdir optConfig optVerbose

  c <- mapM (\c -> maybe (fail $ "collection key not found: " ++ c) return
    $ HMap.lookup (T.pack c) $ configCollections config) optCollection
  t <- getCurrentTime
  d <- generateCollection config (if optForce then Nothing else Just t) c
  mapM_ (outputFile $ \h -> (if optMODS then BSLC.hPut h . outputMODS else BSB.hPutBuilder h . outputPrimo) d) optOutput

  forM_ optServer $ \port -> do
    server port optLog config
