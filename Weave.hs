{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ScopedTypeVariables #-}

module Weave (
    weave
  ) where

import           Prelude hiding (minimum)
import           Control.Monad (mplus)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import           Data.Maybe (fromJust, isJust)
import           Parse

import           Debug.Trace (trace)

debug x = trace (show x) x

data TOCNode = TOCNode B8.ByteString Int [TOCNode]
               deriving (Show)

buildTOC :: [(Int, AWebSection)] -> [TOCNode]
buildTOC sections = fst $ buildTree 0 sections where
  buildTree minLevel = f (minLevel, [])
  f (_, results) [] = (results, [])
  f state@(minLevel, results) all@((sectionNo, (AWebCode { awcLevel = level, awcText = text })) : xs)
    | level > minLevel =
        let (nodes, rest) = buildTree level xs
        in f (minLevel, TOCNode (head text) sectionNo nodes : results) rest
    | level /= 0 && level <= minLevel =
        (results, all)
    | otherwise = f state xs
  f state (x : xs) = f state xs

printTOC :: [TOCNode] -> B8.ByteString
printTOC [] = B8.empty
printTOC nodes = "<ol class=\"toc\">" `B8.append` elems `B8.append` "</ol>" where
  elems = B8.concat $ map f $ reverse nodes
  f (TOCNode text sectionNo subNodes) =
    "<li><a href=\"#section-" `B8.append` B8.pack (show sectionNo) `B8.append` "\">" `B8.append` text `B8.append` "</a>" `B8.append` printTOC subNodes `B8.append` "</li>"

weave :: AWebFile -> [[B8.ByteString]]
weave file = (prelude : (map (weaveSection refMap) numberedSections)) where
  trailer = [["  </body>", "</html>"]]
  refMap = Map.fromList [(name, (sectionNumber, x)) |
                         (sectionNumber, x@(AWebCode { awcCodeName = name })) <-
                         zip [1..] $ awfSections file]
  numberedSections = zip [1..] $ awfSections file
  prelude = map replaceTOC $ awfPrelude file
  replaceTOC "@@TOC" = printTOC $ buildTOC numberedSections
  replaceTOC x = x

maybeDecorateText :: AWebSection -> [B8.ByteString]
maybeDecorateText (AWebCode { awcText = text, awcLevel = level }) = r where
  r = if level == 0
         then text
         else ("<b>" `B8.append` firstLine `B8.append` "</b>") : rest
  firstLine = head text
  rest = tail text

weaveSection refMap (sectionNum, a@(AWebCode { })) =
  [ "    <a name=\"section-" `B8.append` itoa sectionNum `B8.append` "\">"
  , "    <div class=\"section\" id=\"section-" `B8.append` itoa sectionNum `B8.append` "\">"
  , "      <div class=\"sectionnumber\">" `B8.append` itoa sectionNum `B8.append` "</div>"
  ] ++ maybeDecorateText a ++
  [ "      <div class=\"codename\">" `B8.append` awcMsg a `B8.append` "</div>"
  , "      <div class=\"code\">"
  ] ++
  concatMap (weaveCode refMap) (reindent $ tabExpand $ awcCode a) ++
  [ "      </div>"
  , "    </div>"
  ]

weaveSection refMap (sectionNum, a@(AWebOutput { })) =
  [ "    <div class=\"section\" id=\"section-" `B8.append` itoa sectionNum `B8.append` "\">"
  , "      <div class=\"sectionnumber\">" `B8.append` itoa sectionNum `B8.append` "</div>"
  , "      <div class=\"outputfile\">" `B8.append` awoFileName a `B8.append` "</div>"
  , "      <div class=\"code\">"
  ] ++
  concatMap (weaveCode refMap) (reindent $ tabExpand $ awoCode a) ++
  [ "      </div>"
  , "    </div>"
  ]

itoa = B8.pack . show

minimum [] = 0
minimum xs = foldl1 min xs

tabExpand :: [CodeBlockPart] -> [CodeBlockPart]
tabExpand = map f where
  f c@(CodeBlockCode { cbcCode = code }) = c { cbcCode = map (stringReplace '\t' "        ") code }
  f x = x

stringReplace :: Char -> B8.ByteString -> B8.ByteString -> B8.ByteString
stringReplace char replacement s =
  case B8.elemIndex char s of
       Nothing -> s
       Just x -> B8.take x s `B8.append` replacement `B8.append` stringReplace char replacement (B8.drop (x + 1) s)

-- | Reindent a CodeBlock by removing spaces from the beginning of each line
--   such that the least indented line ends up with none. Recall that all tabs
--   have already been expanded by the parser at this point.
reindent :: [CodeBlockPart] -> [CodeBlockPart]
reindent parts = map (drop greatestCommonWhitespace) parts where
  greatestCommonWhitespace = minimum $ map leading parts
  leading (CodeBlockCode { cbcCode = code }) =
    minimum $
    map (B8.length . B8.takeWhile (== ' ')) $
    filter (isJust . B8.findIndex (/= ' '))
    code
  leading (CodeBlockRef { cbrLeading = l }) = l

  drop spaces c@(CodeBlockCode { cbcCode = code }) =
    c { cbcCode = map (B8.drop spaces) code }
  drop spaces c@(CodeBlockRef { cbrLeading = l }) =
    c { cbrLeading = l - spaces }

weaveCode :: Map.Map B8.ByteString (Int, AWebSection) -> CodeBlockPart -> [B8.ByteString]
weaveCode _ (CodeBlockCode { cbcCode = code }) =
  if code == [B8.empty]
     then []
     else map weaveCodeLine code
weaveCode refMap (CodeBlockRef { cbrName = name, cbrLeading = leading }) =
  [ "    <div class=\"ref\">" `B8.append`
    B8.replicate leading ' ' `B8.append`
    "<span class=\"reftext\">" `B8.append`
    awcMsg code `B8.append`
    "</span> <a href=\"#section-" `B8.append`
    itoa sectionNumber `B8.append`
    "\" class=\"refnum\">" `B8.append`
    itoa sectionNumber `B8.append`
    "</a></div>"
  ] where
  (sectionNumber, code) = fromJust $ Map.lookup name refMap

weaveCodeLine line
  | B8.null line = "<pre>\n</pre>"
  | otherwise = "<div class=\"codeline\">" `B8.append` highlight (cEscape line) `B8.append` "</div>"

cEscape s = foldl f s cEscapes where
  f s (char, replacement) = stringReplace char replacement s

cEscapes :: [(Char, B8.ByteString)]
cEscapes = [ ('&', "&amp;")
           , ('<', "&lt;")
           , ('>', "&gt;")
           ]

highlight line
  | B8.head line == '#' = markupPreprocessor line
  | otherwise = B8.concat $ map spanToHTML spans where
      spans = foldl f spans' cKeywords
      f spans keyword = concatMap g spans where
        g (Raw x) = markupResults (runStateMachine (CouldStartWord keyword) x) x
        g x = [x]
      spans' = markupResults smresults line
      smresults = runStateMachine (SMPair NotInString NotInComment NoHold) line

markupPreprocessor :: B8.ByteString -> B8.ByteString
markupPreprocessor line = B8.concat $ map spanToHTML r where
  results = runStateMachine NotInComment line
  spans = markupResults results line
  r = case head spans of
           (Raw s) -> (Preprocessor s) : tail spans
           _ -> spans

data Span = Raw B8.ByteString
          | String B8.ByteString
          | Keyword B8.ByteString
          | Comment B8.ByteString
          | Preprocessor B8.ByteString
          deriving (Show)

spanLength (Raw s) = B8.length s
spanLength (String s) = B8.length s
spanLength (Keyword s) = B8.length s
spanLength (Comment s) = B8.length s

cKeywords = [ "int", "unsigned", "long", "signed", "char", "static", "const", "inline"
            , "struct", "class", "template", "uint8_t", "uint16_t", "uint32_t"
            , "uint64_t", "intptr_t", "uintptr_t", "enum", "for", "while"
            , "case", "switch", "default", "if"
            ]

spanToHTML :: Span -> B8.ByteString
spanToHTML (Raw x) = x
spanToHTML (String x) = "<span class=\"string\">" `B8.append` x `B8.append` "</span>"
spanToHTML (Keyword x) = "<span class=\"keyword\">" `B8.append` x `B8.append` "</span>"
spanToHTML (Comment x) = "<span class=\"comment\">" `B8.append` x `B8.append` "</span>"
spanToHTML (Preprocessor x) = "<span class=\"preprocessor\">" `B8.append` x `B8.append` "</span>"

data SpanResult = StringSpan Int Int
                | LineCommentStart Int
                | KeywordSpan Int Int
                  deriving (Show)

markupResults :: [SpanResult] -> B8.ByteString -> [Span]
markupResults results line = foldl f [Raw line] results where
  f spans (StringSpan start end) = markupSpan String start end spans
  f spans (LineCommentStart start) = markupSpan Comment start (lineLength - 1) spans
  f spans (KeywordSpan start end) = markupSpan Keyword start end spans
  lineLength = B8.length line

markupSpan :: (B8.ByteString -> Span) -> Int -> Int -> [Span] -> [Span]
markupSpan style start end spans = reverse $ snd $ foldl f (0, []) spans where
  f (startIndex, spans) span@(Raw s)
    | startIndex <= start && startIndex + B8.length s > end =
        (startIndex + B8.length s,
         Raw (B8.drop ((start - startIndex) + 1 + (end - start)) s) :
          style (B8.take (end - start + 1) $ B8.drop (start - startIndex) s) :
          Raw (B8.take (start - startIndex) s) : spans)
    | otherwise = (startIndex + B8.length s, span : spans)
  f (startIndex, spans) span =
    (startIndex + spanLength span, span : spans)

class StateMachine x where
  smReset :: x -> x
  smStep :: x -> (Char, Int) -> (x, Bool, Maybe SpanResult)

runStateMachine :: forall a. StateMachine a => a -> B8.ByteString -> [SpanResult]
runStateMachine initState = third . B8.foldl' f (initState, 0, []) . (flip B8.snoc) '\0' where
  third (_, _, x) = x
  f :: (a, Int, [SpanResult]) -> Char -> (a, Int, [SpanResult])
  f (st, index, results) c =
    case smStep st (c, index) of
         (st, _, Nothing) -> (st, index + 1, results)
         (st, _, Just x) -> (st, index + 1, x : results)

data Hold = HoldA | HoldB | NoHold deriving (Show, Eq)
data SMPair a b = SMPair a b Hold

instance forall a b . (StateMachine a, StateMachine b) => StateMachine (SMPair a b) where
  smReset (SMPair a b _) = SMPair (smReset a) (smReset b) NoHold
  smStep (SMPair a b hold) e =
    case hold of
     HoldA ->
      case smStep a e of
           (st', False, result) -> (SMPair st' b NoHold, False, result)
           (st', True, result) -> (SMPair st' (smReset b) HoldA, True, result)
     HoldB ->
      case smStep b e of
           (st', False, result) -> (SMPair a st' NoHold, False, result)
           (st', True, result) -> (SMPair (smReset a) st' HoldB, True, result)
     NoHold ->
      case smStep a e of
           (st', False, result) ->
             case smStep b e of
                  (stb', False, resultb) -> (SMPair st' stb' NoHold, False, result `mplus` resultb)
                  (stb', True, resultb) -> (SMPair (smReset st') stb' HoldB, True, result `mplus` resultb)
           (st', _, result) -> (SMPair st' (smReset b) HoldA, True, result)

data StringScannerState = NotInString
                        | InString Int
                        | InStringEscape Int
                        deriving (Show)

instance StateMachine StringScannerState where
  smReset = const NotInString
  smStep NotInString ('"', i) = (InString i, True, Nothing)
  smStep NotInString _ = (NotInString, False, Nothing)
  smStep (InString start) ('"', i) = (NotInString, False, Just $ StringSpan start i)
  smStep (InString start) ('\\', i) = (InStringEscape start, True, Nothing)
  smStep (InString start) _ = (InString start, True, Nothing)
  smStep (InStringEscape start) _ = (InString start, True, Nothing)

data CommentScannerState = NotInComment
                         | SeenOneSlash
                         | InComment

instance StateMachine CommentScannerState where
  smReset = const NotInComment
  smStep NotInComment ('/', i) = (SeenOneSlash, False, Nothing)
  smStep NotInComment _ = (NotInComment, False, Nothing)
  smStep SeenOneSlash ('/', i) = (InComment, True, Just $ LineCommentStart (i - 1))
  smStep SeenOneSlash _ = (NotInComment, False, Nothing)
  smStep InComment _ = (InComment, True, Nothing)

data KeywordScannerState = CouldStartWord B8.ByteString
                         | InOtherWord B8.ByteString
                         | InKeyword B8.ByteString Int
                         | MatchedWord B8.ByteString
                         deriving (Show)

instance StateMachine KeywordScannerState where
  smReset (CouldStartWord w) = CouldStartWord w
  smReset (InOtherWord w) = CouldStartWord w
  smReset (InKeyword w _) = CouldStartWord w
  smReset (MatchedWord w) = CouldStartWord w

  smStep (CouldStartWord w) (c, i)
    | B8.head w == c =
        if B8.length w == 1
           then (MatchedWord w, False, Nothing)
           else (InKeyword w 1, False, Nothing)
    | c == ' ' = (CouldStartWord w, False, Nothing)
    | c == '(' = (CouldStartWord w, False, Nothing)
    | c == '{' = (CouldStartWord w, False, Nothing)
    | c == '<' = (CouldStartWord w, False, Nothing)
    | c == '\t' = (CouldStartWord w, False, Nothing)
    | otherwise = (InOtherWord w, False, Nothing)
  smStep (InOtherWord w) (' ', _) = (CouldStartWord w, False, Nothing)
  smStep (InOtherWord w) _ = (CouldStartWord w, False, Nothing)
  smStep (InKeyword w nextToMatch) (c, i)
    | c == B8.index w nextToMatch =
        if B8.length w == (nextToMatch + 1)
           then (MatchedWord w, False, Nothing)
           else (InKeyword w (nextToMatch + 1), False, Nothing)
    | otherwise = (InOtherWord w, False, Nothing)
  smStep (MatchedWord w) (c, i)
    | c == ' ' || c == '\0' || c == ':' || c == ')' || c == '}'
        = (CouldStartWord w, False, Just $ KeywordSpan (i - B8.length w) (i - 1))
    | otherwise = (InOtherWord w, False, Nothing)
