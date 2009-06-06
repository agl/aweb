{-# LANGUAGE OverloadedStrings #-}

module Tangle (
    TangleFile
  , TangleOutput(..)
  , TangleCode(..)
  , toTangle
  , tangleOutput
  ) where

import           Control.Monad (mapM_)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import           Parse
import           Text.Printf (printf)

type TangleFile = [TangleOutput]

data TangleOutput = TangleOutput { toFileName :: B8.ByteString
                                 , toBlocks :: [TangleCode]
                                 } deriving (Show)

data TangleCode = TangleCode { tcLineNo :: Int
                             , tcCode :: [B8.ByteString]
                             } deriving (Show)

toTangle :: AWebFile -> TangleFile
toTangle infile = outputs where
  outputs = map buildOutput aWebOutputs
  sections = awfSections infile
  aWebOutputs =
    [(fname, code) | AWebOutput { awoFileName = fname, awoCode = code } <- sections]
  buildOutput (fname, code) = TangleOutput fname $ expandCodeBlock nmap [] code
  nmap = Map.fromList [(name, s) | s@(AWebCode { awcCodeName = name }) <- sections]

expandCodeBlock nmap history cb =
  concatMap f cb where
  f :: CodeBlockPart -> [TangleCode]
  f CodeBlockCode { cbcLineNo = lineno, cbcCode = code } = [TangleCode lineno code]
  f CodeBlockRef { cbrName = name, cbrLineNo = lineno } = r where
    r = case history `findPrefixEndingIn` referenced of
             Just x -> error $ printf "Cycle found: %s" $ printCycle x
             Nothing -> expandCodeBlock nmap (referenced : history) $ awcCode referenced
    referenced = case Map.lookup name nmap of
                      Nothing -> error $ printf "line %d: Reference to unknown section" lineno
                      Just x -> x

findPrefixEndingIn :: (Eq a) => [a] -> a -> Maybe [a]
findPrefixEndingIn [] _ = Nothing
findPrefixEndingIn (x:xs) y
  | x == y = Just [x]
  | otherwise = case findPrefixEndingIn xs y of
                     Nothing -> Nothing
                     Just xs -> Just (x : xs)

printCycle :: [AWebSection] -> String
printCycle = show . map f where
  f (AWebCode { awcLineNo = lineno }) = "Section at line " ++ show lineno

tangleSerialise :: B8.ByteString  -- ^ Source filename or empty to omit preprocessor lines
                -> [TangleCode]
                -> B8.ByteString
tangleSerialise sourceFileName codes =
  if B8.null sourceFileName
     then B8.intercalate "\n" $ concatMap tcCode codes
     else B8.intercalate "\n" $ concatMap (\c -> (B8.pack ("#line " ++ show (tcLineNo c) ++ " \"") `B8.append` sourceFileName `B8.append` B8.singleton '"') : tcCode c) codes

tangleOutput :: B8.ByteString -> TangleFile -> IO ()
tangleOutput sourceFileName = mapM_ (\c -> B8.writeFile (B8.unpack $ toFileName c) $ tangleSerialise sourceFileName $ toBlocks c)
