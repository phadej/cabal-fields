{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RecordWildCards #-}
module CabalFields.FFI.Hsc where

#include "cabalfields.h"

import Data.Word (Word8)

import Foreign.Storable
import Foreign.Ptr

import Foreign.C.String
import Foreign.C.Types

data SrcLoc = SrcLoc CInt CInt
  deriving (Eq, Show)

instance Storable SrcLoc where
    sizeOf _ = #{size cf_srcloc}
    alignment _ = #{alignment cf_srcloc}

    peek ptr = do
        row <- #{peek cf_srcloc, row} ptr
        col <- #{peek cf_srcloc, col} ptr
        return (SrcLoc row col)

    poke ptr (SrcLoc row col) = do
        #{poke cf_srcloc, row} ptr row
        #{poke cf_srcloc, col} ptr col

data TokenType
    = TkTyUnexpectedChar
    | TkTyInconsistentIndentation
    | TkTyStackOverflow
    | TkTyEOF
    | TkTyComment
    | TkTyField
    | TkTyFieldLine
    | TkTySection
    | TkTyFieldEnd
    | TkTySectionEnd
    | TkTySkip
  deriving (Eq, Ord, Show)

tokenTypeFromCInt :: CInt -> Maybe TokenType
tokenTypeFromCInt (#{const CF_TK_UNEXPECTED_CHAR})          = Just TkTyUnexpectedChar
tokenTypeFromCInt (#{const CF_TK_INCONSISTENT_INDENTATION}) = Just TkTyInconsistentIndentation
tokenTypeFromCInt (#{const CF_TK_STACKOVERFLOW})            = Just TkTyStackOverflow
tokenTypeFromCInt #{const CF_TK_EOF}                        = Just TkTyEOF
tokenTypeFromCInt #{const CF_TK_COMMENT}                    = Just TkTyComment
tokenTypeFromCInt #{const CF_TK_FIELD}                      = Just TkTyField
tokenTypeFromCInt #{const CF_TK_FIELDLINE}                  = Just TkTyFieldLine
tokenTypeFromCInt #{const CF_TK_FIELD_END}                  = Just TkTyFieldEnd
tokenTypeFromCInt #{const CF_TK_SECTION}                    = Just TkTySection
tokenTypeFromCInt #{const CF_TK_SECTION_END}                = Just TkTySectionEnd
#ifdef CF_HAS_TK_SKIP
tokenTypeFromCInt #{const CF_TK_SKIP}                       = Just TkTySkip
#endif
tokenTypeFromCInt _ = Nothing

data Token
    = TkUnexpectedChar SrcLoc CString Word8
    | TkInconsistentIndentation SrcLoc
    | TkStackOverflow SrcLoc
    | TkEOF SrcLoc CSize
    | TkComment SrcLoc CSize CSize
    | TkField SrcLoc CSize CSize SrcLoc
    | TkFieldLine SrcLoc CSize CSize
    | TkSection SrcLoc CSize CSize SrcLoc CSize CSize
    | TkFieldEnd
    | TkSectionEnd
    | TkSkip
  deriving (Eq, Show)

tokenType :: Token -> TokenType
tokenType TkUnexpectedChar {}          = TkTyUnexpectedChar
tokenType TkInconsistentIndentation {} = TkTyInconsistentIndentation
tokenType TkStackOverflow {}           = TkTyStackOverflow
tokenType TkEOF {}                     = TkTyEOF
tokenType TkComment {}                 = TkTyComment
tokenType TkField {}                   = TkTyField
tokenType TkFieldLine {}               = TkTyFieldLine
tokenType TkSection {}                 = TkTySection
tokenType TkFieldEnd                   = TkTyFieldEnd
tokenType TkSectionEnd                 = TkTySectionEnd
tokenType TkSkip                       = TkTySkip

sizeOfToken :: Int
sizeOfToken = #{size cf_token}

peekToken :: TokenType -> Ptr Token -> IO Token
peekToken TkTyUnexpectedChar ptr = do
    loc <- #{peek cf_token, error_uc.loc} ptr
    str <- #{peek cf_token, error_uc.state} ptr
    c   <- #{peek cf_token, error_uc.c} ptr
    return $ TkUnexpectedChar loc str c

peekToken TkTyInconsistentIndentation ptr = do
    loc <- #{peek cf_token, error_ii.loc} ptr
    return $ TkInconsistentIndentation loc

peekToken TkTyStackOverflow ptr = do
    loc <- #{peek cf_token, error_so.loc} ptr
    return $ TkStackOverflow loc

peekToken TkTyEOF ptr = do
    loc <- #{peek cf_token, eof.loc} ptr
    pos <- #{peek cf_token, eof.pos} ptr
    return $ TkEOF loc pos

peekToken TkTyComment ptr = do
    loc <- #{peek cf_token, comment.loc} ptr
    bgn <- #{peek cf_token, comment.bgn} ptr
    end <- #{peek cf_token, comment.end} ptr

    return $ TkComment loc bgn end

peekToken TkTyField ptr = do
    name_loc <- #{peek cf_token, field.name_loc} ptr
    name_bgn <- #{peek cf_token, field.name_bgn} ptr
    name_end <- #{peek cf_token, field.name_end} ptr

    colon_loc <- #{peek cf_token, field.colon_loc} ptr

    return $ TkField name_loc name_bgn name_end colon_loc

peekToken TkTyFieldLine ptr = do
    loc <- #{peek cf_token, fieldline.line_loc} ptr
    bgn <- #{peek cf_token, fieldline.line_bgn} ptr
    end <- #{peek cf_token, fieldline.line_end} ptr

    return $ TkFieldLine loc bgn end

peekToken TkTySection ptr = do
    name_loc <- #{peek cf_token, section.name_loc} ptr
    name_bgn <- #{peek cf_token, section.name_bgn} ptr
    name_end <- #{peek cf_token, section.name_end} ptr

    args_loc <- #{peek cf_token, section.args_loc} ptr
    args_bgn <- #{peek cf_token, section.args_bgn} ptr
    args_end <- #{peek cf_token, section.args_end} ptr

    return $ TkSection name_loc name_bgn name_end args_loc args_bgn args_end

peekToken TkTySectionEnd _ = return TkSectionEnd
peekToken TkTyFieldEnd   _ = return TkFieldEnd
peekToken TkTySkip       _ = return TkSkip 

data ParserState

sizeOfParserState :: Int
sizeOfParserState = #{size cf_parser_state}

peekParserStatePos :: Ptr ParserState -> IO CSize
peekParserStatePos = #{peek cf_parser_state, pos}
