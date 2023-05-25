{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoFieldSelectors #-}
module CabalFields.FFI (
    parseTokens,
    parseSection,
    makeS,
    parseNext,
    S,
    splice,
    rest,
) where

import Data.Word (Word8)
import Data.Char (chr)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Primitive
import Control.Monad.Primitive (PrimState)

import Foreign.C.Types (CSize (..), CInt (..))
import Foreign.Ptr

import qualified Data.ByteString as BS
import qualified Distribution.Parsec.Position as C

import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

import qualified CabalFields.Tokens as T
import CabalFields.FFI.Hsc

parseTokens :: ByteString -> T.Tokens C.Position ByteString String
parseTokens bs = unsafePerformIO $ do
    s <- makeS bs
    parseTop s

mkABS :: S -> SrcLoc -> CSize -> CSize -> T.AnnByteString C.Position
mkABS s loc bgn end = T.ABS (srcLocToPos loc) (splice s bgn end)

srcLocToPos :: SrcLoc -> C.Position
srcLocToPos (SrcLoc r c) = C.Position (cintToInt r) (cintToInt c)

-- | 'parseTop' and 'parseSection' are almost the same.
--
-- The top section is terminated by 'TkEOF', while inner sections
-- are terminated by 'TkSectionEnd'.
--
-- This way we can just iterate tokens until we get 'TkEOF',
-- is our goal is only to tokenize the input.
--
parseTop :: S -> IO (T.Tokens C.Position ByteString String)
parseTop s = do
    token <- parseNext s
    case token of
        TkSection name_loc name_bgn name_end args_loc args_bgn args_end -> do
            ts <- unsafeInterleaveIO $ parseSection s $ parseTop s
            return $ T.TkSection (mkABS s name_loc name_bgn name_end) (mkABS s args_loc args_bgn args_end) ts

        TkEOF _ pos -> do
            return $ T.TkEnd (rest s pos)

        TkField name_loc name_bgn name_end colon_pos -> do
            ts <- unsafeInterleaveIO $ parseTkFieldLines s $ parseTop s
            return $ T.TkField (mkABS s name_loc name_bgn name_end) (srcLocToPos colon_pos) ts

        TkComment loc bgn end -> do
            ts <- unsafeInterleaveIO $ parseTop s
            return $ T.TkComment (mkABS s loc bgn end) ts

        TkSkip -> parseTop s

        _ -> return $ T.TkErr $ show token

parseSection :: S -> IO k -> IO (T.Tokens C.Position k String)
parseSection s kont = do
    token <- parseNext s
    case token of
        TkSection name_loc name_bgn name_end args_loc args_bgn args_end -> do
            ts <- unsafeInterleaveIO $ parseSection s $ parseSection s kont
            return $ T.TkSection (mkABS s name_loc name_bgn name_end) (mkABS s args_loc args_bgn args_end) ts

        TkSectionEnd -> T.TkEnd <$> kont

        TkField name_loc name_bgn name_end colon_pos -> do
            ts <- unsafeInterleaveIO $ parseTkFieldLines s $ parseSection s kont
            return $ T.TkField (mkABS s name_loc name_bgn name_end) (srcLocToPos colon_pos) ts

        TkComment loc bgn end -> do
            ts <- unsafeInterleaveIO $ parseSection s kont
            return $ T.TkComment (mkABS s loc bgn end) ts

        TkSkip -> parseSection s kont

        _ -> return $ T.TkErr $ show token

parseTkFieldLines :: S -> IO k -> IO (T.TkFieldLines C.Position k String)
parseTkFieldLines s kont = do
    token <- parseNext s
    case token of
        TkFieldLine loc bgn end -> do
            ts <- unsafeInterleaveIO $ parseTkFieldLines s kont
            return $ T.TkFieldLine (mkABS s loc bgn end) ts
        TkFieldEnd -> T.TkFieldEnd <$> kont

        TkComment loc bgn end -> do
            ts <- unsafeInterleaveIO $ parseTkFieldLines s kont
            return $ T.TkFieldComment (mkABS s loc bgn end) ts

        TkEOF pos _ -> return $ T.TkFieldErr $ "Unexpected end-of-input at " ++ "TODO"

        TkField pos _ _ _ -> return $ T.TkFieldErr  "unexpected field"
        TkSection pos _ _ _ _ _ -> return $ T.TkFieldErr  "unexpected section"
        TkSectionEnd -> return $ T.TkFieldErr  "unexpected section end"

        TkUnexpectedChar pos _ c -> return $ T.TkFieldErr $ "unexpected char " ++ show (chr (fromIntegral c)) ++ " at " ++ show pos
        TkInconsistentIndentation {} -> return $ T.TkFieldErr  "inconsistent indentation"
        TkStackOverflow {} -> return $ T.TkFieldErr  "stack overflow"

        TkSkip -> parseTkFieldLines s kont



data S = S
    { parserBA :: MutableByteArray (PrimState IO)
    , tokenBA  :: MutableByteArray (PrimState IO)
    , input    :: ByteString
    }

splice :: S -> CSize -> CSize -> ByteString
splice S { input = bs } bgn end
    | bgn <= end = BS.take (csizeToInt end - csizeToInt bgn) $ BS.drop (csizeToInt bgn) bs
    | otherwise  = BS.empty

rest :: S -> CSize -> ByteString
rest S { input = bs} bgn = BS.drop (csizeToInt bgn) bs

parserStatePtr :: S -> Ptr ParserState
parserStatePtr S { parserBA = ba } = castPtr $ mutableByteArrayContents ba

tokenPtr :: S -> Ptr Token
tokenPtr S { tokenBA = ba } = castPtr $ mutableByteArrayContents ba

makeS :: ByteString -> IO S
makeS bs = unsafeUseAsCString bs $ \ptr -> do
    parserBA' <- newPinnedByteArray sizeOfParserState
    tokenBA'  <- newPinnedByteArray sizeOfToken
    cf_reset (castPtr (mutableByteArrayContents parserBA')) (castPtr ptr) (intToCSize (BS.length bs))

    return S
        { parserBA = parserBA'
        , tokenBA  = tokenBA'
        , input    = bs
        }

intToCSize :: Int -> CSize
intToCSize = toEnum

csizeToInt :: CSize -> Int
csizeToInt = fromEnum

cintToInt :: CInt -> Int
cintToInt = fromEnum

parseNext :: S -> IO Token
parseNext s = do
    let t = tokenPtr s
    i <- cf_next (parserStatePtr s) t
    -- t'' <- freezeByteArray t' 0 sizeOfToken
    -- print t''
    case tokenTypeFromCInt i of
        Nothing -> fail "TODO: return error TOKEN"
        Just ty -> peekToken ty t

foreign import capi "cabalfields.h cf_reset"
    cf_reset :: Ptr ParserState -> Ptr Word8 -> CSize -> IO ()

foreign import capi "cabalfields.h cf_next"
    cf_next :: Ptr ParserState -> Ptr Token -> IO CInt
