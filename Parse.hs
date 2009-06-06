{-# LANGUAGE OverloadedStrings #-}

module Parse (
    AWebFile(..)
  , AWebSection(..)
  , CodeBlock
  , CodeBlockPart(..)
  , Line
  , parse
  ) where

import qualified Data.ByteString.Char8 as B8
import           Data.List (unfoldr)
import           Text.Printf (printf)

data AWebFile = AWebFile { awfPrelude :: [B8.ByteString]
                         , awfSections :: [AWebSection]
                         } deriving (Show)

data AWebSection = AWebCode { awcText :: [B8.ByteString]
                            , awcLineNo :: Int
                            , awcCodeName :: B8.ByteString
                            , awcMsg :: B8.ByteString
                            , awcCode :: CodeBlock
                            , awcLevel :: Int
                            }
                 | AWebOutput { awoFileName :: B8.ByteString
                              , awoLineNo :: Int
                              , awoCode :: CodeBlock
                              }
                 deriving (Show)

instance Eq AWebSection where
  (AWebCode { awcLineNo = lineno1 }) == (AWebCode { awcLineNo = lineno2 }) = lineno1 == lineno2
  _ == _ = False

type CodeBlock = [CodeBlockPart]

data CodeBlockPart = CodeBlockCode { cbcLineNo :: Int
                                   , cbcCode :: [B8.ByteString]
                                   }
                   | CodeBlockRef { cbrLineNo :: Int
                                  , cbrName :: B8.ByteString
                                  , cbrLeading :: Int
                                  }
                   deriving (Show)

type Line = (B8.ByteString, Int)

parse :: B8.ByteString -> AWebFile
parse input = file where
  lines = zip (B8.lines input) [1..]
  (prelude, rest) = splitPrelude lines
  sections = map parseSection $ unfoldr readSection rest
  file = AWebFile (fromLines prelude) sections

fromLines :: [Line] -> [B8.ByteString]
fromLines = map fst

-- | Split the prelude from the top of the input by taking lines until
--   we hit the start of a section
splitPrelude :: [Line] -> ([Line], [Line])
splitPrelude = span (not . isSectionStart)

-- | Sections are started by @/ or @{ at the start of a line
isSectionStart :: Line -> Bool
isSectionStart (line, _) = "@/" `B8.isPrefixOf` line ||
                           "@{file " `B8.isPrefixOf` line

readSection :: [Line] -> Maybe ([Line], [Line])
readSection [] = Nothing
readSection (x:xs) = result where
  (a, b) = span (not . isSectionStart) xs
  result = Just (x : a, b)

parseSection :: [Line] -> AWebSection
parseSection ((header, lineno) : rest)
  | "@{file " `B8.isPrefixOf` header =
      parseFileSection lineno (chomp $ B8.drop 7 header) rest
  | "@/" `B8.isPrefixOf` header =
      parseOutputSection lineno $ ((header, lineno) : rest)
  | otherwise = error $ printf "Didn't expect: %s" (show header)

isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\t' = True
isWhitespace '\n' = True
isWhitespace '\r' = True
isWhitespace _ = False

chomp :: B8.ByteString -> B8.ByteString
chomp s = B8.dropWhile isWhitespace $ B8.reverse chompedReversed where
  chompedReversed = B8.dropWhile isWhitespace reversed
  reversed = B8.reverse s

parseFileSection :: Int -> B8.ByteString -> [Line] -> AWebSection
parseFileSection lineno fileName lines =
  AWebOutput fileName lineno code where
  code = parseCode lines

-- | Let the first line pass, but then remove all empty lines from the start
--   and end of the list.
stripExtraBlankLines :: [Line] -> [Line]
stripExtraBlankLines [] = error "stripExtraBlankLines called on empty list"
stripExtraBlankLines (first:rest) = first : rest' where
  rest' = removeLeadingBlankLines rest
  removeLeadingBlankLines [] = []
  removeLeadingBlankLines (x:xs)
    | fst x == "" = removeLeadingBlankLines xs
    | otherwise = x : removeTrailingBlankLines xs
  removeTrailingBlankLines [] = []
  removeTrailingBlankLines (x:xs)
    | fst x == "" =
        case removeTrailingBlankLines xs of
             [] -> []
             y -> x : y
    | otherwise = x : removeTrailingBlankLines xs

parseOutputSection :: Int -> [Line] -> AWebSection
parseOutputSection lineno lines =
  AWebCode text lineno name msg code level where
  (textLines, codeLines) = span (not . isStartOfCode) lines
  headerLine = fst $ head textLines
  level = B8.length $ B8.takeWhile (== '*') $ B8.drop 2 headerLine
  codeLines' = stripExtraBlankLines codeLines
  isStartOfCode (line, _) = "@<" `B8.isPrefixOf` line
  text = (B8.drop 1 $ B8.dropWhile (/= ' ') $ headerLine) : (fromLines $ tail textLines)
  (name, msg) =
    if null codeLines'
       then error $ printf "line %d: No code block in section" lineno
       else parseCodeName $ head codeLines'

  parseCodeName (line, lineno) =
    if not ("@>=" `B8.isSuffixOf` line)
       then error $ printf "line %d: Expected @>= at end of line" lineno
       else let inner = B8.drop 2 $ B8.take (B8.length line - 3) line
             in case B8.elemIndex '|' inner of
                     Nothing -> (inner, inner)
                     Just i -> (B8.take i inner, B8.drop (i + 1) inner)

  code = parseCode $ tail codeLines'

takeCodeLines :: [Line] -> ([Line], [Line])
takeCodeLines = span (not . isRefLine) where
  isRefLine (s, _) = "@<" `B8.isPrefixOf` (B8.dropWhile isWhitespace s)

parseCode :: [Line] -> [CodeBlockPart]
parseCode = unfoldr f where
  f [] = Nothing
  f lines = part where
    (a, rest) = takeCodeLines lines
    part = if null a
              then Just (parseCodeBlockRef $ head rest, tail rest)
              else Just (CodeBlockCode (snd $ head a) $ fromLines a, rest)

whitespaceLength :: B8.ByteString -> Int
whitespaceLength = B8.foldl' f 0 where
  f c ' ' = c + 1
  f c '\t' = c + 8

parseCodeBlockRef (s, lineno) = CodeBlockRef lineno inner leading where
  inner = if not ("@>" `B8.isSuffixOf` s)
             then error $ printf "line %d: Expected @> at end of line" lineno
             else B8.drop 2 $ B8.init $ B8.init $ B8.dropWhile isWhitespace s
  leading = whitespaceLength $ B8.takeWhile isWhitespace s
