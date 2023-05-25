{-# LANGUAGE OverloadedStrings #-}
module CabalFields.Encoding (
    encodeTokens,
) where

import Data.Void (Void, absurd)

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Builder      as B
import qualified Data.ByteString.Lazy         as LBS
import qualified Distribution.Parsec.Position as C

import CabalFields.Internal.Input

import CabalFields.Tokens


encodeTokens :: Tokens C.Position k Void -> LBS.ByteString
encodeTokens tokens = B.toLazyByteString (buildTokens (\_ _ -> mempty) (C.Position 1 1) tokens)

buildTokens :: (C.Position -> k -> B.Builder) -> C.Position -> Tokens C.Position k Void -> B.Builder
buildTokens kont pos (TkEnd k) = kont pos k
buildTokens _    _   (TkErr e) = absurd e
buildTokens kont pos (TkComment (ABS pos' bs) ts) = buildSpace pos (posMinus2 pos') <> "--" <> B.byteString bs <> "\n" <> buildTokens kont (posRet pos') ts
buildTokens kont pos (TkSection (ABS pos' name) (ABS pos'' args) ts)
    = buildSpace pos pos'
    <> B.byteString name
    <> buildSpace (advancePos pos' name) pos''
    <> B.byteString args
    <> "\n"
    <> buildTokens (buildTokens kont) (posRet pos'') ts
buildTokens kont pos (TkField (ABS pos' name) pos'' fls)
    = buildSpace pos pos'
    <> B.byteString name
    <> buildSpace (advancePos pos' name) pos''
    <> ":"
    <> buildFieldLines (buildTokens kont) (advancePos pos'' ":") fls

buildFieldLines :: (C.Position -> k -> B.Builder) -> C.Position -> TkFieldLines C.Position k Void -> B.Builder
buildFieldLines kont pos (TkFieldEnd k) = "\n" <> kont (posRet pos) k
buildFieldLines _    _   (TkFieldErr e) = absurd e
buildFieldLines kont pos (TkFieldComment (ABS pos' bs) fls) = buildSpace pos  (posMinus2 pos') <> "--" <> B.byteString bs <> "\n" <> buildFieldLines' kont (posRet pos') fls
buildFieldLines kont pos (TkFieldLine (ABS pos' bs) fls)
    = buildSpace pos pos'
    <> B.byteString bs
    <> "\n"
    <> buildFieldLines' kont (posRet pos') fls

buildFieldLines' :: (C.Position -> k -> B.Builder) -> C.Position -> TkFieldLines C.Position k Void -> B.Builder
buildFieldLines' kont pos (TkFieldEnd k) = kont pos k
buildFieldLines' _    _   (TkFieldErr e) = absurd e
buildFieldLines' kont pos (TkFieldComment (ABS pos' bs) fls) = buildSpace pos  (posMinus2 pos') <> "--" <> B.byteString bs <> "\n" <> buildFieldLines' kont (posRet pos') fls
buildFieldLines' kont pos (TkFieldLine (ABS pos' bs) fls)
    = buildSpace pos pos'
    <> B.byteString bs
    <> "\n"
    <> buildFieldLines' kont (posRet pos') fls

buildSpace :: C.Position -> C.Position -> B.Builder
buildSpace pos@(C.Position r c) pos'@(C.Position r' c') = case compare r' r of
    GT ->  "\n" <> buildSpace (posRet pos) pos'
    EQ -> if c' > c then B.byteString (BS.replicate (c' - c) 0x20) else mempty
    LT -> mempty

posRet :: C.Position -> C.Position
posRet (C.Position r _) = C.Position (r + 1) 1

posMinus2 :: C.Position -> C.Position
posMinus2 (C.Position r c) = C.Position r (c - 2)
