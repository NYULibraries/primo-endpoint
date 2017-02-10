{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Exception (handle, IOException)
import           Control.Monad (forM_)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Char (isDigit)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

data Source
  = SourceFDA String
  deriving (Eq, Show)

sourceOpts :: [Opt.OptDescr Source]
sourceOpts =
  [ Opt.Option "f" ["fda"] (Opt.ReqArg SourceFDA "SRC")
    "Load FDA OAI_DC records(s) from XML file, URL, or hdl"
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
loadFile hdl@('h':'d':'l':'_':'2':'4':'5':'1':'_':(all isDigit -> True)) =
  loadFile $ "https://archive.nyu.edu/request?verb=ListRecords&metadataPrefix=oai_dc&set=" ++ hdl
loadFile (HTTP.parseUrlThrow -> Just q) =
  HTTP.responseBody <$> HTTP.httpLBS q
loadFile f = BSLC.readFile f

outputFile :: String -> BSLC.ByteString -> IO ()
outputFile "-" = BSLC.putStr
outputFile f = BSLC.writeFile f

writeJSON :: String -> [JSON.Value] -> IO ()
writeJSON f = outputFile f . JSON.encode

loadSource :: Source -> IO BSLC.ByteString
loadSource (SourceFDA f) = do
  bs <- loadFile f
  -- either fail return $ JSON.parseEither (parseFedoraResponse Nothing) =<< JSON.eitherDecode bs
  return bs

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
  -- nyu <- nyuCoreSchema
  -- langs <- loadLanguages
  forM_ optSources $ \s ->
    handle (\e -> hPutStrLn stderr (show s ++ ": " ++ show (e :: IOException)))
      $ loadSource s >> return ()
