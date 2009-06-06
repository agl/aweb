module Main where

import           Control.Applicative ((<$>))
import           Control.Monad (forM, when)
import qualified Data.ByteString.Char8 as B8
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.FilePath (replaceExtension)
import           System.IO (openFile, hClose, IOMode(..))
import           Text.Printf (printf)

import           Parse
import           Tangle
import           Weave

data Options = Options { optLocationLines :: Bool
                       } deriving (Show)

options :: [OptDescr (Options -> Options)]
options = [
    Option [] ["no-location-lines"]
      (NoArg (\opts -> opts { optLocationLines = False }))
      "Do not include #line in output files"
  ]

defaultOptions = Options { optLocationLines = True }

getOptions :: IO (Options, String)
getOptions = do
  argv <- getArgs
  case getOpt Permute options argv of
       (opts, [fileName], []) -> return (foldl (flip id) defaultOptions opts, fileName)
       (_, _ : _ : _, []) -> error "Unexpected arguments given."
       (_, [], []) -> error "Please give input file as final argument."
       (_, _, errs) -> error ((concat errs) ++ usageInfo "" options)

main = do
  (options, inputFile) <- getOptions
  input <- parse <$> B8.readFile inputFile
  tangleOutput (if optLocationLines options then B8.pack inputFile else B8.empty) $ toTangle input
  htmlOutput <- openFile (inputFile `replaceExtension` ".html") WriteMode
  mapM (mapM (B8.hPutStrLn htmlOutput)) $ weave input
  hClose htmlOutput
