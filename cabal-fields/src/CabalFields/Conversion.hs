{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
module CabalFields.Conversion (
    toEitherFields,
    parseSectionArgs,
) where

import qualified Data.ByteString              as BS
import qualified Distribution.Fields.Field    as C
import qualified Distribution.Parsec.Position as C

import CabalFields.Internal.Input
import CabalFields.Internal.Misc
import CabalFields.Internal.Sasha (satthI)
import CabalFields.Tokens

import qualified CabalFields.Internal.ERE as ERE

-------------------------------------------------------------------------------
-- To Fields
-------------------------------------------------------------------------------

mkName :: AnnByteString ann -> C.Name ann
mkName (ABS ann bs) = C.mkName ann bs

mkFieldLine :: AnnByteString ann -> C.FieldLine ann
mkFieldLine (ABS ann bs) = C.FieldLine ann bs

toEitherFields :: Tokens C.Position k e -> Either e ([C.Field C.Position], k)
toEitherFields tks = goS emptyDList tks Left (\res k -> Right (res, k)) where
    goS :: DList (C.Field C.Position) -> Tokens C.Position k e -> (e -> r) -> ([C.Field C.Position] -> k -> r) -> r
    goS !acc (TkEnd k)          _ r = r (runDList acc) k
    goS !_   (TkErr err)        e _ = e err
    goS !acc (TkComment _ ls)   e r = goS acc ls e r
    goS !acc (TkField n _ ls)   e r = goF emptyDList ls e $ \fls k -> goS (snocDList acc $ C.Field (mkName n) fls) k e r
    goS !acc (TkSection n l tk) e r = goS emptyDList tk e $ \fs  k -> goS (snocDList acc $ C.Section (mkName n) (parseSectionArgs l) fs) k e r -- TODO: section args

    goF :: DList (C.FieldLine ann) -> TkFieldLines ann k e -> (e -> r) -> ([C.FieldLine ann] -> k -> r) -> r
    goF !acc (TkFieldEnd k) _      r = r (runDList acc) k
    goF !_   (TkFieldErr err)      e _ = e err
    goF !acc (TkFieldLine l ls)    e r = goF (snocDList acc (mkFieldLine l)) ls e r
    goF !acc (TkFieldComment _ ls) e r = goF acc ls e r

type DList a = [a] -> [a]

emptyDList :: DList a
emptyDList = id
{-# INLINE emptyDList #-}

snocDList :: DList a -> a -> DList a
snocDList xs x = xs . (x :)
{-# INLINE snocDList #-}

runDList :: DList a -> [a]
runDList xs = xs []
{-# INLINE runDList #-}

-------------------------------------------------------------------------------
-- Section arguments
-------------------------------------------------------------------------------

parseSectionArgs :: AnnByteString C.Position -> [C.SectionArg C.Position]
parseSectionArgs (ABS ann bs0) = go (Input bs0 ann)
  where
    trimStr :: BS.ByteString -> BS.ByteString
    trimStr bs = BS.take (BS.length bs - 2) (BS.tail bs)

    go inp = $$(satthI
        [|| [] ||]
        [ ERE.indent := \_ inp' -> [|| go $$inp' ||]
        , ERE.name   := \t inp' -> [|| C.SecArgName  (inpPos inp) $$t : go $$inp' ||]
        , ERE.paren' := \t inp' -> [|| C.SecArgOther (inpPos inp) $$t : go $$inp' ||]
        , ERE.oplike := \t inp' -> [|| C.SecArgOther (inpPos inp) $$t : go $$inp' ||]
        , ERE.string := \t inp' -> [|| C.SecArgStr   (inpPos inp) (trimStr $$t) : go $$inp' ||]
        ])
        inp
