{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module CabalFields.Decoding (
    -- * Parsing function
    parseTokens,
    -- * Parser type
    Parser,
    Parser',
    Parser'',
    -- ** Stack
    Stack,
    emptyStack,
    pushStack,
    showStack,
    -- * Productions
    parseB0,
    parseBI,
    parseBC,
    parseBQ,
    parseS0,
    parseAN,
    parseSA,
    parseAC,
    parseFN,
    parseF0,
    -- ** Popping stack
    popStackName,
    popStackIndentName,
    popStackEOF,
) where

-- Uncomment for debug traces
-- #define DEBUG

#ifdef DEBUG
import Debug.Trace
import Text.Printf (printf)
#endif

import qualified CabalFields.Internal.ERE     as ERE
import qualified Data.ByteString              as BS
import qualified Distribution.Parsec.Position as C

import CabalFields.Internal.Input
import CabalFields.Internal.Misc
import CabalFields.Internal.Sasha (memberCode, satthW)
import CabalFields.Tokens

debug :: String -> Stack k -> Input -> b -> b
#ifdef DEBUG
debug t stk inp x =
    trace (printf "%03d:%02d %2s %15s %s" r c t (showStack stk) (show (BS.take 30 (inpBS inp)))) $
    x
  where
    C.Position r c = inpPos inp
#else
debug _ _ _ x = x
#endif

mkABS :: Input -> BS.ByteString -> AnnByteString C.Position
mkABS inp t = ABS (inpPos inp) t

type Error = String

unexpected :: String -> BS.ByteString -> Error
unexpected tag bs = tag ++ " unexpected: " ++ show bs

-- | Root production.
--
-- The same as @'parseS' 0@ but also recognises UTF8-BOM.
parseTokens :: BS.ByteString -> Tokens C.Position BS.ByteString Error
parseTokens bs = parseB0 (emptyStack inpBS) (mkInput bs)

-- | Parser
type Parser  t k = Stack k -> Input -> t C.Position k Error

-- | Parser expecting a thing on top of the stack.
type Parser' t k = Parser'' (t C.Position k Error)

type Parser'' k  = Stack k -> Input -> k

-------------------------------------------------------------------------------
-- Continuation stack.
-------------------------------------------------------------------------------

data Stack k where
    StackInput :: (Input -> k) -> Stack k
    StackS     :: !Int -> !(Stack k) -> Stack (Tokens C.Position k Error)

emptyStack :: (Input -> k) -> Stack k
emptyStack = StackInput

pushStack :: Int -> Stack k -> Stack (Tokens C.Position k Error)
pushStack = StackS

showStack :: Stack k -> String
showStack = show . go where
    go :: Stack k -> [Int]
    go (StackInput _) = []
    go (StackS i stk) = i : go stk

offsetOfStack :: Stack k -> Int
offsetOfStack (StackInput _) = (-1)
offsetOfStack (StackS i _)   = i

-------------------------------------------------------------------------------
-- Popping from stack
-------------------------------------------------------------------------------

{- | Same as 'popStack' at input position with name at the beginning of the line.

@
PopName ::= PopNameIndent(0)
@

Called at zero position.

-}
popStackName :: Input -> AnnByteString C.Position -> Parser'' k
popStackName inp0 = popStackIndentName inp0 0

{- | Pop stack after indentation and a name.

@
PopNameIndent(j) ::= AfterName        ; stk@(i:_), i == j
                  |  PopNameIndent(j) ; stk@(i:_), i >  j, pop
@

-}
popStackIndentName :: Input -> Int -> AnnByteString C.Position -> Parser'' k
popStackIndentName inp0 j n stk inp = debug "PI" stk inp (popStackIndentName' inp0 j n stk inp)

popStackIndentName' :: Input -> Int -> AnnByteString C.Position -> Parser'' k
popStackIndentName' inp0 _ _     (StackInput k)  _   = k inp0
popStackIndentName' inp0 j n stk@(StackS i stk') inp = case compare i j of
    GT -> TkEnd (popStackIndentName inp0 j n stk' inp)
    EQ -> parseAN j n stk $ inpSkipSpace inp
    LT -> TkErr $ "Inconsistent indentation" ++ show (j, i)

-- | Pop stack at end-of-file.
popStackEOF :: Parser'' k
popStackEOF (StackInput k) inp = k inp
popStackEOF (StackS _ stk) inp = TkEnd (popStackEOF stk inp)

-------------------------------------------------------------------------------
-- Parsing functions: beginning
-------------------------------------------------------------------------------

{- | The beginning of the file.

@
Begin            ::= name   ws AfterName              ; push 0
                  |  ws(j)  BeginIndent(j)
                  |  nl     Begin
                  |  "--"   BeginComment
@

-}
parseB0 :: Parser Tokens k
parseB0 !stk !inp = debug "B0" stk inp $ $$(satthW
    [|| TkEnd (popStackEOF stk inp) ||]
    [ ERE.name         := \ t inp' -> [|| parseAN 0 (mkABS inp $$t) (pushStack 0 stk) $ inpSkipSpace $$inp' ||]
    , ERE.indent       := \ t inp' -> [|| parseBI (BS.length $$t) stk $$inp'                                 ||]
    , ERE.newline      := \ _ inp' -> [|| parseB0 stk $$inp'                                                 ||]
    , ERE.commentHead  := \ _ inp' -> [|| parseBC stk $$inp'                                                 ||]
    , ERE.unexpected   := \ e _    -> [|| TkErr (unexpected "B0" $$e)                                        ||]
    ])
    inp

{- | After the beginning indent

@
BeginIndent(j)   ::= nl   Begin
                  |  "--" BeginComment
                  |  name ws AfterName                ; push j
@

-}
parseBI :: Int -> Parser Tokens k
parseBI !j !stk !inp = debug "BI" stk inp $ $$(satthW
    [|| TkEnd (popStackEOF stk inp) ||]
    [ ERE.newline      := \ _ inp' -> [|| parseB0 stk $$inp' ||]
    , ERE.commentHead  := \ _ inp' -> [|| parseBC stk $$inp' ||]
    , ERE.name         := \ t inp' -> [|| parseAN j (mkABS inp $$t) (pushStack j stk) $ inpSkipSpace $$inp' ||]
    , ERE.unexpected   := \ e _    -> [|| TkErr (unexpected "BI" $$e) ||]
    ])
    inp

{- | While at the beginning after comment start @--@ token.

@
BeginComment     ::= comment* BeginCommentNL
@

-}
parseBC :: Parser Tokens k
parseBC !stk !inp = debug "BC" stk inp $ case inpSpan (\w8 -> $$(memberCode [|| w8 ||] ERE.commentCS)) inp of
    (pfx, inp') -> TkComment (mkABS inp pfx) (parseBQ stk inp')

{- | While at the beginning after comment contents.

@
BeginCommentNL   ::= nl Begin
@

-}
parseBQ :: Parser Tokens k
parseBQ !stk !inp = debug "BQ" stk inp $$(satthW
    [|| TkEnd (popStackEOF stk inp) ||]
    [ ERE.newline      := \_ inp' -> [|| parseB0 stk $$inp' ||]
    , ERE.unexpected   := \e _    -> [|| TkErr (unexpected "BQ" $$e) ||]
    ])
    inp

-------------------------------------------------------------------------------
-- Parsing functions: after name and after colon
-------------------------------------------------------------------------------

{- | After colon (and whitespace).

@
AfterColon       ::= fieldline FieldLineNL
                  |  nl        FieldLine
@

-}
parseAC :: Parser TkFieldLines k
parseAC !stk !inp = debug "AC" stk inp $ $$(satthW
    [|| TkFieldEnd (popStackEOF stk inp) ||]
    [ ERE.fieldline    := \ t inp' -> [|| TkFieldLine (mkABS inp $$t) (parseFN stk $$inp') ||]
    , ERE.newline      := \ _ inp' -> [|| parseF0 stk $$inp'                               ||]
    , ERE.unexpected   := \ e _    -> [|| TkFieldErr (unexpected "AC" $$e)                 ||]
    ])
    inp

{- | After a name (and whitespace)

@
AfterName        ::= ":"         ws AfterColon
                  |  nl          Section
                  |  sectionArgs AfterSectionArgs
@

-}
parseAN :: Int -> AnnByteString C.Position -> Parser' Tokens k
parseAN !_ !name !stk !inp = debug "AN" stk inp $ $$(satthW
    [|| TkSection name (mkABS inp BS.empty) (TkEnd (popStackEOF stk inp)) ||]
    [ ERE.colon        := \ _ inp' -> [|| TkField   name (inpPos inp)         $ parseAC stk $ inpSkipSpace $$inp' ||]
    , ERE.newline      := \ _ inp' -> [|| TkSection name (mkABS inp BS.empty) $ parseS0 stk $$inp'                ||]
    , ERE.sectionArgs  := \ t inp' -> [|| TkSection name (mkABS inp $$t)      $ parseSA stk $$inp'                ||]
    , ERE.unexpected   := \ e _    -> [|| TkErr (unexpected "AN" $$e)                                             ||]
    ])
    inp

parseAN' :: Input -> Int -> AnnByteString C.Position -> Parser'' k
parseAN' !inp0 !_ !_ !(StackInput k)   !_   = k inp0
parseAN' !_    !j !n !stk@(StackS _ _) !inp = parseAN j n stk inp

-------------------------------------------------------------------------------
-- Parsing functions: sections
-------------------------------------------------------------------------------

{- | Immediately after section header. After a newline (at initial column).
Looking for the first content (and its indent).

@
Section          ::= name      PopName
                  |  indent(j) ...
                  |  nl        Section
                  |  "--"      SectionComment
@

-}
parseS0 :: Parser Tokens k
parseS0 !stk !inp = debug "S0" stk inp $ $$(satthW
    [|| TkEnd (popStackEOF stk inp) ||]
    [ ERE.name         := \ t inp' -> [|| TkEnd $ popStackName inp (mkABS inp $$t) stk $$inp' ||]
    , ERE.indent       := \ t inp' -> [|| do let j = BS.length $$t
                                             if j > i
                                             then parseSG inp j stk $$inp'
                                             else parseSL inp j stk $$inp' ||]
    , ERE.newline      := \ _ inp' -> [|| parseS0                                  stk $$inp' ||]
    , ERE.commentHead  := \ _ inp' -> [|| parseSC                                  stk $$inp' ||]
    , ERE.unexpected   := \ e _    -> [|| TkErr (unexpected "S0" $$e)                         ||]
    ])
    inp
  where
    !i = offsetOfStack stk

{- | After section arguments. Expecting newline.

@
AfterSectionArgs ::= nl Section
@

-}
parseSA :: Parser Tokens k
parseSA !stk !inp = debug "SA" stk inp $ $$(satthW
    [|| TkEnd (popStackEOF stk inp) ||]
    [ ERE.newline      := \ _ inp' -> [|| parseS0 stk $$inp'          ||]
    , ERE.unexpected   := \ e _    -> [|| TkErr (unexpected "SA" $$e) ||]
    ])
    inp

-- | While parsing section contents after indent larger than current.
parseSG :: Input -> Int -> Parser Tokens k
parseSG inp0 !j !stk !inp = debug "SG" stk inp $ $$(satthW
    [|| TkEnd (popStackEOF stk inp) ||]
    [ ERE.newline      := \ _ inp' -> [|| parseS0 stk $$inp' ||]
    , ERE.commentHead  := \ _ inp' -> [|| parseSC stk $$inp' ||]
    , ERE.name         := \ t inp' -> [|| parseAN j (mkABS inp $$t) (pushStack j stk) $ inpSkipSpace $$inp' ||]
    , ERE.unexpected   := \ e _    -> [|| TkErr (unexpected "SG" $$e) ||]
    ])
    inp

parseSL :: Input -> Int -> Parser Tokens k
parseSL inp0 !j !stk !inp = debug "SL" stk inp $ $$(satthW
    [|| TkEnd (popStackEOF stk inp) ||]
    [ ERE.newline      := \ _ inp' -> [|| parseS0 stk $$inp' ||]
    , ERE.commentHead  := \ _ inp' -> [|| parseSC stk $$inp' ||]
    , ERE.name         := \ t inp' -> [|| TkEnd $ popStackIndentName inp0 j (mkABS inp $$t) stk $$inp' ||]
    , ERE.unexpected   := \ e _    -> [|| TkErr (unexpected "SL" $$e) ||]
    ])
    inp

-- | While parsing section contents after comment start @--@ token.
parseSC :: Parser Tokens k
parseSC !stk !inp = debug "SC" stk inp $ case inpSpan (\w8 -> $$(memberCode [|| w8 ||] ERE.commentCS)) inp of
    (pfx, inp') -> TkComment (mkABS inp pfx) (parseSQ stk inp')

-- | While parsing sections after comment content. Expecting newline.
parseSQ :: Parser Tokens k
parseSQ !stk !inp =  debug "SQ" stk inp $$(satthW
    [|| TkEnd (popStackEOF stk inp) ||]
    [ ERE.newline      := \ _ inp' -> [|| parseS0 stk $$inp' ||]
    , ERE.unexpected   := \ e _    -> [|| TkErr (unexpected "SQ" $$e) ||]
    ])
    inp

-------------------------------------------------------------------------------
-- Parsing functions: fields
-------------------------------------------------------------------------------

{- | After colon and newline or fieldline and newline. After a newline (at initial column).

@
FieldLine        ::= indent(j)  ...                      ; j == i
                  |  "--"       FieldLineComment
                  |  nl         FieldLine
                  |  name       ...
@

-}
parseF0 :: Parser TkFieldLines k
parseF0 !stk !inp = debug "F0" stk inp $ $$(satthW
    [|| TkFieldEnd (popStackEOF stk inp) ||]
    [ ERE.indent       := \ t inp' -> [|| do let j = BS.length $$t
                                             if j > offsetOfStack stk
                                             then parseFG       stk $$inp'
                                             else parseFL inp j stk $$inp' ||]
    , ERE.commentHead  := \ _ inp' -> [|| parseFC stk $$inp'||]
    , ERE.newline      := \ _ inp' -> [|| parseF0 stk $$inp'||]
    , ERE.name         := \ t inp' -> [|| TkFieldEnd (popStackName inp (mkABS inp $$t) stk $$inp') ||]
    , ERE.unexpected   := \ e _    -> [|| TkFieldErr (unexpected "F0" $$e) ||]
    ])
    inp

{- After a fieldline, expecting newline.

@
FieldLineNL      ::= nl FieldLine
@

-}
parseFN :: Parser TkFieldLines k
parseFN !stk !inp = debug "FN" stk inp $ $$(satthW
    [|| TkFieldEnd (popStackEOF stk inp) ||]
    [ ERE.newline      := \ _ inp' -> [|| parseF0 stk $$inp'               ||]
    , ERE.unexpected   := \ e _    -> [|| TkFieldErr (unexpected "FN" $$e) ||]
    ])
    inp

-- | While parsing field lines after indent larger than current.
parseFG :: Parser TkFieldLines k
parseFG !stk !inp = debug "FI" stk inp $ $$(satthW
    [|| TkFieldEnd (popStackEOF stk inp) ||]
    [ ERE.newline      := \ _ inp' -> [|| parseF0 stk $$inp' ||]
    , ERE.commentHead  := \ _ inp' -> [|| parseFC stk $$inp' ||]
    , ERE.fieldline'   := \ t inp' -> [|| TkFieldLine (mkABS inp $$t) (parseFN stk $$inp') ||]
    , ERE.unexpected   := \ e _    -> [|| TkFieldErr (unexpected "FI" $$e) ||]
    ])
    inp

-- | While parsing field lines after indent smaller or equal to current: @indent <= indentOfStack@
parseFL
    :: Input
    -> Int -- ^ indent size
    -> Parser TkFieldLines k
parseFL !inp0 !j !stk !inp = debug "FL" stk inp $ $$(satthW
    [|| TkFieldEnd (popStackEOF stk inp) ||]
    [ ERE.newline      := \ _ inp' -> [|| parseF0 stk $$inp' ||]
    , ERE.commentHead  := \ _ inp' -> [|| parseFC stk $$inp' ||]
    , ERE.name         := \ t inp' -> [|| TkFieldEnd $ popStackIndentName inp0 j (mkABS inp $$t) stk $$inp' ||]
    , ERE.unexpected   := \ e _    -> [|| TkFieldErr (unexpected "FL" $$e) ||]
    ])
    inp

-- | While parsing field lines after comment start @--@ token.
parseFC :: Parser TkFieldLines k
parseFC !stk !inp = debug "FC" stk inp $ case inpSpan (\w8 -> $$(memberCode [|| w8 ||] ERE.commentCS)) inp of
    (pfx, inp') -> TkFieldComment (mkABS inp pfx) (parseFQ stk inp')

-- | While parsing field lines after comment content. Expecting newline.
parseFQ :: Parser TkFieldLines k
parseFQ !stk !inp = debug "FQ" stk inp $$(satthW
    [|| TkFieldEnd (popStackEOF stk inp) ||]
    [ ERE.newline      := \ _ inp' -> [|| parseF0 stk $$inp' ||]
    , ERE.unexpected   := \ e _    -> [|| TkFieldErr (unexpected "FQ" $$e) ||]
    ])
    inp
