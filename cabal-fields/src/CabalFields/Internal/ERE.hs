{-# LANGUAGE OverloadedStrings #-}
module CabalFields.Internal.ERE (
    -- * Character sets
    nameCS,
    commentCS,
    sectionArgsHeadCS,
    sectionArgsTailCS,
    fieldlayoutCS,
    -- * For parsing fields
    name,
    indent,
    colon,
    newline,
    sectionArgs,
    commentHead,
    commentTail,
    fieldline,
    fieldline',
    unexpected,
    -- * For parsing section args
    string,
    oplike,
    paren',
) where

import Algebra.Lattice ((/\), (\/))
import Sasha.TTH       (ERE)

import Data.Word8Set (Word8Set, (\\))

import qualified Data.Word8Set      as W8S
import qualified Sasha.Internal.ERE as ERE

-- $setup
--
-- >>> :set -XTemplateHaskellQuotes
-- >>> import Sasha.Internal.Word8Set (memberCode)
-- >>> import Language.Haskell.TH (Code, Q, unTypeCode, runQ)
-- >>> import Language.Haskell.TH.Ppr (ppr)
--
-- >>> let printCode :: Code Q a -> IO (); printCode c = runQ (unTypeCode c) >>= print . ppr
--
-- >>> import Data.Word (Word8)
-- >>> import Data.Word8Set as W8S
--
-- >>> eqW8S :: W8S.Word8Set -> (Char -> Bool) -> IO (); eqW8S expected p = let actual = W8S.fromASCII [ c | c <- ['\0'..'\xff'], p c ] in if actual == expected then return () else print expected >> print actual
--

{-
$space           = \          -- single space char
$ctlchar         = [\x0-\x1f \x7f]
$printable       = \x0-\xff # $ctlchar   -- so no \n \r
$symbol'         = [ \, \= \< \> \+ \* \& \| \! \$ \% \^ \@ \# \? \/ \\ \~ ]
$symbol          = [$symbol' \- \.]
$spacetab        = [$space \t]
-}

space, ctlchar, printable, symbol', symbol, spacetab :: Word8Set

space            = " "
ctlchar          = W8S.range 0x00 0x1f \/ W8S.singleton 0x7f
printable        = W8S.complement ctlchar
symbol'          = ",=<>+*&|!$%^@#?/\\~"
symbol           = symbol' \/ "-."
spacetab         = space \/ "\t"

{-
$paren           = [ \( \) \[ \] ]
$field_layout    = [$printable \t]
$field_layout'   = [$printable] # [$space]
$field_braces    = [$printable \t] # [\{ \}]
$field_braces'   = [$printable] # [\{ \} $space]
$comment         = [$printable \t]
$namecore        = [$printable] # [$space \: \" \{ \} $paren $symbol']
$instr           = [$printable $space] # [\"]
$instresc        = $printable
-}

paren, fieldlayoutCS, commentCS, nameCS, instr, instresc :: Word8Set

paren            = "()[]"

-- | Field content characters.
--
-- >>> eqW8S fieldlayoutCS $ \c -> c == '\t' || ('\x20' <= c && c < '\x7f') || c >= '\x80'
fieldlayoutCS    = printable \/ "\t"

-- | Comment characters.
--
-- >>> eqW8S commentCS $ \c -> c == '\t' || ('\x20' <= c && c < '\x7f') || c >= '\x80'
--
commentCS        = printable \/ "\t"

-- | Name characters
--
-- >>> eqW8S nameCS $ \c -> c == '\'' || c == '-' || c == '.' || ('0' <= c && c <= '9') || c == ';' || ('A' <= c && c <= 'Z') || ('_' <= c && c < '{') || c >= '\x80'
nameCS           = printable \\ W8S.unions [space, ":\"{}", paren, symbol']

-- | Initial section arg character
--
-- >>> eqW8S sectionArgsHeadCS $ \c -> (' ' <= c && c < '\127' && c /= ':' && c /= '{' && c /= '}') || c >= '\x80'
sectionArgsHeadCS :: Word8Set
sectionArgsHeadCS   = printable \\ "{}:"

-- | Trailing section arg character
--
-- >>> eqW8S sectionArgsTailCS $ \c -> (' ' <= c && c < '\127' && c /= '{' && c /= '}') || c >= '\x80'
sectionArgsTailCS :: Word8Set
sectionArgsTailCS    = printable \\ "{}"


instr            = (printable \/ space) \\ "\""
instresc         = printable

{-
@bom          = \xef \xbb \xbf
@nbsp         = \xc2 \xa0
@nbspspacetab = ($spacetab | @nbsp)
@nbspspace    = ($space | @nbsp)
@nl           = \n | \r\n | \r
@name         = $namecore+
@string       = \" ( $instr | \\ $instresc )* \"
@oplike       = $symbol+
-}

newline, name, paren', string, oplike :: ERE

newline       = ERE.unions ["\n", "\r\n", "\r"]
name          = ERE.plus (ERE.charSet nameCS) /\ ERE.complement ("--" <> ERE.star ERE.anyChar)
paren'        = ERE.charSet paren
string        = "\"" <> ERE.star (ERE.charSet instr \/ "\\" <> ERE.charSet instresc) <> "\""
oplike        = ERE.plus (ERE.charSet symbol)

colon :: ERE
colon = ":"

fieldline :: ERE
fieldline = ERE.plus (ERE.charSet fieldlayoutCS)

-- | Fieldline which doesn't start with comment starter.
fieldline' :: ERE
fieldline' = ERE.plus (ERE.charSet fieldlayoutCS) /\ ERE.complement (commentHead <> ERE.everything)

sectionArgs :: ERE
sectionArgs = ERE.charSet sectionArgsHeadCS <> ERE.star (ERE.charSet sectionArgsTailCS)

commentHead :: ERE
commentHead = "--"

commentTail :: ERE
commentTail = ERE.plus (ERE.charSet commentCS)

indent :: ERE
indent = ERE.plus (ERE.charSet spacetab)

unexpected :: ERE
unexpected = ERE.anyChar \/ ERE.anyUtf8Char
