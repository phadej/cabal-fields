{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Applicative        (liftA2)
import Control.Monad              (forM_, unless)
import Data.Foldable              (foldl')
import Data.Functor.Representable
import Data.List                  (dropWhileEnd)
import Data.TreeDiff.Class        (ediff)
import Data.TreeDiff.Pretty       (ansiWlEditExpr)
import Data.Void                  (absurd)
import GHC.Generics               (Generic, Generic1)
import Optics.Core                ((%~), (&))
import Test.Tasty                 (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit           (assertFailure, testCaseSteps)
import Text.Printf                (printf)

import qualified Cabal.Config                           as CI
import qualified Cabal.Index                            as CI
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.ByteString.Lazy.Char8             as LBS8
import qualified Distribution.Fields.Field              as C
import qualified Distribution.PackageDescription.Quirks as C
import qualified Distribution.Parsec.Position           as C

import qualified CabalFields.Compat as CF

import CabalFields.Conversion (toEitherFields)
import CabalFields.Decoding   (parseTokens)
import CabalFields.Encoding   (encodeTokens)
import qualified CabalFields.FFI as FFI

import Orphans ()

main :: IO ()
main = defaultMain $ testGroup "cabal-fields-ref"
    [ testGroup "comparison"
        [ comparison "comment.txt"
        , comparison "comment_bi.txt"
        , comparison "comment_s0.txt"
        , comparison "comment_sg.txt"
        , comparison "comment_sl.txt"
        , comparison "comment_f0.txt"
        , comparison "comment_fg.txt"
        , comparison "comment_fl.txt"
        , comparison "empty.txt"
        , comparison "empty-sections.txt"
        , comparison "eof_ac.txt"
        , comparison "eof_an.txt"
        , comparison "eof_bi.txt"
        , comparison "eof_bn.txt"
        , comparison "eof_fg.txt"
        , comparison "eof_fl.txt"
        , comparison "eof_fn.txt"
        , comparison "eof_fq.txt"
        , comparison "eof_sa.txt"
        , comparison "eof_sg.txt"
        , comparison "eof_sl.txt"
        , comparison "eof_sq.txt"
        , comparison "multi-line-fields.txt"
        , comparison "nested-sections.txt"
        , comparison "nl_bi.txt"
        , comparison "nl_fg.txt"
        , comparison "nl_fl.txt"
        , comparison "nl_sg.txt"
        , comparison "nl_sl.txt"
        , comparison "section-args.txt"
        , comparison "semicolon-field.txt"
        , comparison "semicolon-section.txt"
        , comparison "simple-sections.txt"
        , comparison "single-line-fields.txt"
        ]

    , testCaseSteps "hackage" $ \info -> do
        let process :: CI.IndexEntry -> BS.ByteString -> Count -> IO Count
            process entry contents' count
                | CI.CabalFile {} <- CI.entryType entry
                = do
                      let (_, contents) = C.patchQuirks contents'

                      (cab, ws) <- case CF.readFields' contents of
                          Left err -> assertFailure (show entry ++ show err)
                          Right fs -> return fs

                      if | not $ null ws -> return $ count
                            & #cWarns    %~ (+1)
                            & #cPerWarns %~ liftA2 (+) (perLW ws)

                         | CI.entryPath entry `elem` skipThese -> return $ count
                            & #cSkipped %~ (+1)

                         | otherwise -> do
                          let tks = parseTokens contents
                          let ffi = FFI.parseTokens contents

                          unless (tks == ffi) $ assertFailure $ unlines
                            [ "pure and ffi tokens differ"
                            , show entry
                            , show tks
                            , show ffi 
                            ]

                          ref <- case toEitherFields tks of
                              Left err        -> assertFailure $ show (count, entry) ++ err
                              Right (fs, bs')
                                  | BS.null bs' -> return fs
                                  | otherwise   -> assertFailure $ "Non-null trailing bytestring" ++ show entry ++ show bs'

                          if ref == cab
                          then do
                            let zeroIndent = case cab of
                                    [] -> True
                                    C.Field   (C.Name (C.Position _ c) _) _   : _ -> c == 1
                                    C.Section (C.Name (C.Position _ c) _) _ _ : _ -> c == 1
                            let count'
                                    | zeroIndent = count
                                    | otherwise  = count & #cNonZI %~ (+1)

                            return $ count'
                                & #cTotal %~ (+1)
                          else do
                              -- print cab
                              -- print ref
                              print (count, entry)
                              assertFailure $ show $ ansiWlEditExpr $ ediff cab ref

                | otherwise
                = return count

        cfg <- CI.readConfig

        indexPath <- maybe (fail "No Hackage?") return $ CI.cfgRepoIndex cfg CI.hackageHaskellOrg
        info indexPath

        count <- CI.foldIndex indexPath count0 process
        info $ printf "Processed files: %6d" (cTotal count)
        info $ printf "Non-zero-indent: %6d" (cNonZI count)
        info $ printf "Skipped files:   %6d" (cSkipped count)
        info $ printf "Skipped w files: %6d" (cWarns count)

        forM_ [minBound .. maxBound] $ \wt -> do
          info $ printf "                 %6d %s" (index (cPerWarns count) wt) (show wt)
    ]

data Count = Count
    { cTotal    :: !Int
    , cSkipped  :: !Int
    , cNonZI    :: !Int
    , cWarns    :: !Int
    , cPerWarns :: !(PerLW Int)
    }
  deriving (Show, Generic)

count0 :: Count
count0 = Count 0 0 0 0 (pure 0)

-- we need https://github.com/haskell/cabal/issues/9098
-- the next `Cabal-syntax` will however warn about inconsistent indentation.
data PerLW a = PerLW !a !a !a !a !a
  deriving (Functor, Show, Generic, Generic1)

perLW :: [CF.LexWarning] -> PerLW Int
perLW = foldl' f (pure 0) where
    f :: PerLW Int -> CF.LexWarning -> PerLW Int
    f acc (CF.LexWarning w _) = tabulate $ \i -> if w == i then 1 else index acc i

instance Representable CF.LexWarningType PerLW where
    index = gindex
    tabulate = gtabulate

instance Applicative PerLW where
    pure = repPure
    (<*>) = repAp

skipThese :: [FilePath]
skipThese =
    [ "patch/0.0.5.1/patch.cabal"
    , "patch/0.0.5.2/patch.cabal"
    , "patch/0.0.6.0/patch.cabal"
    , "patch/0.0.7.0/patch.cabal"
    , "patch/0.0.8.0/patch.cabal"
    , "patch/0.0.8.1/patch.cabal"
    ]

comparison :: TestName -> TestTree
comparison name = testCaseSteps name $ \info -> do
    contents <- BS.readFile ("fixtures/ok/" ++ name)

    cab <- case CF.readFields contents of
        Left err -> assertFailure (show err)
        Right fs -> return fs

    let tokens = parseTokens contents
    tokens' <- traverse assertFailure tokens

    -- print tokens

    let contents' = encodeTokens tokens'
    unless (contents' == normalise contents) $ do
        info "encode different"
        LBS8.writeFile "encoded.txt" contents'


    let ffi = FFI.parseTokens contents
    ffi' <- traverse assertFailure ffi

    if tokens' == ffi'
    then return ()
    else do
        assertFailure $ unlines
            [ "pure and ffi tokens differ:"
            , show tokens'
            , show ffi'
            ]

    ref <- case toEitherFields tokens' of
        Left err        -> absurd err
        Right (fs, bs')
            | BS.null bs' -> return fs
            | otherwise   -> assertFailure "Non-null trailing bytestring"
    if ref == cab
    then return ()
    else do
        -- print cab
        -- print ref
        assertFailure $ show $ ansiWlEditExpr $ ediff cab ref

normalise :: BS.ByteString -> LBS.ByteString
normalise
    = LBS8.unlines
    . dropWhileEnd LBS8.null
    . map (LBS8.dropWhileEnd (== ' '))
    . LBS8.lines
    . LBS8.fromStrict
