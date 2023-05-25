{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative (liftA2, many, (<**>))
import Data.Foldable (asum)
import Control.Monad (when, forM_)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafeInterleaveIO)

import CabalFields.FFI
import CabalFields.Tokens.PP
import CabalFields.FFI.Hsc
import qualified CabalFields.Tokens as T
import qualified CabalFields.Decoding as D
import qualified Distribution.Fields.Field as C
import qualified Data.ByteString as BS
import qualified Options.Applicative as O

data Mode
    = ModeTokens
    | ModeFfiTokens

optsP :: O.Parser Mode
optsP = asum
    [ O.flag' ModeTokens    $ O.long "tokens" <> O.help "Parse tokens"
    , O.flag' ModeFfiTokens $ O.long "ffi-tokens" <> O.help "Parse tokens with ffi lib" 
    , pure ModeTokens
    ]

argP :: O.Parser FilePath
argP = O.strArgument $ O.metavar "FILE" <> O.help "Input file"

main :: IO ()
main = do
  (opts, args) <- O.execParser $ O.info (liftA2 (,) optsP (many argP) <**> O.helper) O.fullDesc
  case opts of
      ModeTokens -> forM_ args $ \arg -> do
          contents <- BS.readFile arg
          let tokens = D.parseTokens contents
          printTokens tokens

      ModeFfiTokens -> forM_ args $ \arg -> do
          contents <- BS.readFile arg
          let tokens = parseTokens contents
          printTokens tokens
