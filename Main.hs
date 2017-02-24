{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Exception (throwIO, handle, SomeException)
import           Control.Monad (forM_, when)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import qualified Data.Yaml as YAML
import           Network.Connection (TLSSettings(..))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           Config
import           Document
import           Server

data Opts = Opts
  { optConfig :: FilePath
  , optOutput :: String
  , optServer :: Maybe Int
  }

defOpts :: Opts
defOpts = Opts
  { optConfig = "config.yml"
  , optOutput = "-"
  , optServer = Nothing
  }

opts :: [Opt.OptDescr (Opts -> Opts)]
opts =
  [ Opt.Option "c" ["config"] (Opt.ReqArg (\f o -> o{ optConfig = f }) "FILE")
    ("Load configuration from FILE [" ++ optConfig defOpts ++ "]")
  , Opt.Option "o" ["output"] (Opt.OptArg (\f o -> o{ optOutput = (fromMaybe "-" f) }) "DEST")
    "Write JSON output to file [-]"
  , Opt.Option "w" ["web-server"] (Opt.OptArg (\f o -> o{ optServer = Just (maybe 80 read f) }) "PORT")
    "Run a web server on PORT [80] to serve the result"
  ]

outputFile :: String -> BSLC.ByteString -> IO ()
outputFile "-" = BSLC.putStr
outputFile f = BSLC.writeFile f

writeOutput :: String -> [Document] -> IO ()
writeOutput f = outputFile f . JSON.encode

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
  
  config <- either throwIO return =<< YAML.decodeFileEither optConfig

  HTTPS.setGlobalManager =<< HTTP.newManager (HTTPS.mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  forM_ (configCollections config) $ \c -> handle
    (\e -> hPutStrLn stderr (show (collectionSource c) ++ ": " ++ show (e :: SomeException)))
    $ writeOutput optOutput =<< loadSource (collectionSource c)

  forM_ optServer $ \port -> do
    when (optOutput == "-") $ fail "Web server requires output file path."
    server port optOutput
