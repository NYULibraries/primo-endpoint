{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Exception (handle, IOException)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Char (isDigit)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           Document
import           FDA

data Source
  = SourceFDA String
  deriving (Eq, Show)

sourceOpts :: [Opt.OptDescr Source]
sourceOpts =
  [ Opt.Option "f" ["fda"] (Opt.ReqArg SourceFDA "SRC")
    "Load FDA collections from JSON file, URL, or ID"
  ]

data Opts = Opts
  { optSources :: [Source]
  , optOutput :: String
  }

defOpts :: Opts
defOpts = Opts
  { optSources = []
  , optOutput = "-"
  }

opts :: [Opt.OptDescr (Opts -> Opts)]
opts =
  [ Opt.Option "o" ["output"] (Opt.ReqArg (\f o -> o{ optOutput = f }) "DEST")
    "Write MODS output to file, substituting \"$vars\" [-]"
  ] ++ map (fmap (\s o -> o{ optSources = s : optSources o })) sourceOpts

loadFile :: String -> IO BSLC.ByteString
loadFile "-" = BSLC.getContents
loadFile i@(all isDigit -> True) =
  loadFile $ "https://archive.nyu.edu/rest/collections/" ++ i ++ "/items?expand=metadata"
loadFile (HTTP.parseUrlThrow -> Just q) =
  HTTP.responseBody <$> HTTP.httpLBS q
loadFile f = BSLC.readFile f

outputFile :: String -> BSLC.ByteString -> IO ()
outputFile "-" = BSLC.putStr
outputFile f = BSLC.writeFile f

writeOutput :: String -> [Document] -> IO ()
writeOutput f = outputFile f . JSON.encode

loadSource :: Source -> IO [Document]
loadSource (SourceFDA f) = do
  bs <- loadFile f
  either fail return $ JSON.parseEither readFDA =<< JSON.eitherDecode bs

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  Opts{..} <- case Opt.getOpt Opt.Permute opts args of
    (ol, [], []) -> return $ foldr ($) defOpts ol
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]") opts
      exitFailure
  writeOutput optOutput . concat
    =<< mapM (\s -> handle (\e -> [] <$ hPutStrLn stderr (show s ++ ": " ++ show (e :: IOException)))
      $ loadSource s) optSources 
