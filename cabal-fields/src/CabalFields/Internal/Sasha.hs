{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module CabalFields.Internal.Sasha (
    satthI,
    satthW,
    memberCode,
) where

import Algebra.Lattice            ((\/))
import Control.Monad              (forM_, unless, when)
import Data.Char                  (chr, ord)
import Data.Word                  (Word8)
import Data.Word8Set              (Word8Set)
import Language.Haskell.TH.Syntax (Code, Q, joinCode, reportWarning)
import Sasha.Internal.Word8Set    (memberCode)
import Sasha.TTH                  (ERE, satth)
import Text.Printf                (printf)

import CabalFields.Internal.Input

import qualified Data.ByteString    as BS
import qualified Data.Word8Set      as W8S
import qualified Sasha.Internal.ERE as ERE

satthI
    :: Code Q r                                                   -- ^ no match
    -> [(ERE, Code Q BS.ByteString -> Code Q Input -> Code Q r)]  -- ^ rules
    -> Code Q (Input -> r)
satthI noMatch rules =
    [|| \(Input bs _pos) -> $$(satth noMatch [ (ere, \t bs' -> f t [|| Input $$bs' (advancePos _pos $$t) ||]) | (ere, f) <- rules ]) bs ||]

satthW
    :: Code Q r                                                   -- ^ no match
    -> [(ERE, Code Q BS.ByteString -> Code Q Input -> Code Q r)]  -- ^ rules
    -> Code Q (Input -> r)
satthW noMatch rules = joinCode $ do
    forM_ rules $ \(r, _) -> when (ERE.nullable r) $ do
        reportWarning $ "nullable rule: " ++ show r

    fs <- paraM leading W8S.empty rules

    unless (W8S.isFull fs) $
        reportWarning "rules are not maching exhaustively"

    return (satthI noMatch rules)
  where
    leading w (r, _) rs = do
        let n = ereLeadingSet r
        if W8S.isFull n && null rs -- for each Word8, derivative w r is nullable.
        then return n
        else do
            unless (W8S.disjoint w n) $ reportWarning $ "leading chars are not disjoint with previous: " ++ ppW8S (W8S.intersection w n)
            return (w \/ n)

paraM :: Monad m => (b -> a -> [a] -> m b) -> b -> [a] -> m b
paraM _ !acc []     = return acc
paraM f  acc (x:xs) = f acc x xs >>= \acc' -> paraM f acc' xs

ereLeadingSet :: ERE -> Word8Set
ereLeadingSet (ERE.EREAppend []) = W8S.empty
ereLeadingSet (ERE.EREAppend (r:rs))
    | ERE.nullable r = ereLeadingSet r \/ ereLeadingSet (ERE.EREAppend rs)
    | otherwise      = ereLeadingSet r
ereLeadingSet (ERE.EREUnion w8s rs) = w8s \/ foldMap ereLeadingSet rs
ereLeadingSet (ERE.EREStar r) = ereLeadingSet r
ereLeadingSet (ERE.ERENot r) = W8S.complement (ereLeadingSet r)

ppW8S :: Word8Set -> String
ppW8S w8s = case W8S.toList w8s of
    []   -> "[]"
    [w]  -> ppChar (fromWord8 w)
    w:ws -> "[" ++ ppChar (fromWord8 w) ++ (go w ws)
  where
    go _ [] = "]"
    go w (u:ws) = if w + 1 == u then go' u ws else ppChar (fromWord8 u) ++ go u ws

    go' w [] = "-" ++ ppChar (fromWord8 w) ++ "]"
    go' w (u:ws) = if w + 1 == u then go' u ws else "-" ++ ppChar (fromWord8 w) ++ ppChar (fromWord8 u) ++ go u ws

fromWord8 :: Word8 -> Char
fromWord8 = chr . fromIntegral

ppChar :: Char -> String
ppChar c
    | c == '\f'          = "\\f"
    | c == '\r'          = "\\r"
    | c == '\n'          = "\\n"
    | c == '\t'          = "\\t"
    | c <= '\x1F'        = printf "\\x  %02x" (ord c)
    | c `elem` metaChars = "\\" ++ [c]
    | c == '\xFF'        = "\\xff"
    | otherwise          = [c]

metaChars :: String
metaChars = "()[]+*.-^\\|&"

-- \d [0-9]
-- \D [^0-9]
-- \w [A-Za-z0-9_]
-- \W [^[A-Za-z0-9_]
-- \s [\f\n\r\t \xa0]
-- \S ...
-- \t \r \n \v \f
