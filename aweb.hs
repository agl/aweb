module Main where

import           Control.Applicative ((<$>))
import           Control.Monad (forM, when)
import qualified Data.ByteString.Char8 as B8
import           System.Environment (getArgs)
import           System.IO (openFile, hClose, IOMode(..))
import           Text.Printf (printf)

import           Parse
import           Tangle
import           Weave

main = do
  [inputFile] <- getArgs
  input <- parse <$> B8.readFile inputFile
  tangleOutput (B8.pack inputFile) $ toTangle input
  -- tangleOutput B8.empty $ toTangle input
  htmlOutput <- openFile "output.html" WriteMode
  mapM (mapM (B8.hPutStrLn htmlOutput)) $ weave input
  hClose htmlOutput
