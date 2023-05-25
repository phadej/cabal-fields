module CabalFields.Tokens.PP (
    printTokens,
) where

import qualified Distribution.Fields.Field as C

import CabalFields.Tokens

-- | Print tokens on multiple lines.
printTokens :: (Show k, Show e, Show ann) => Tokens ann k e -> IO ()
printTokens = printTokens' print

printTokens' :: (Show e, Show ann) => (k -> IO ()) -> Tokens ann k e -> IO ()
printTokens' kont (TkSection name args ts) = do
    putStrLn $ showString "TkSection " . showsPrec 11 name . showChar ' ' . showsPrec 11 args . showString " $" $ ""
    printTokens' (printTokens' kont) ts
printTokens' kont (TkField name colon ts) = do
    putStrLn $ showString "TkField " . showsPrec 11 name . showChar ' ' . showsPrec 11 colon $ ""
    printTkFieldLines' (printTokens' kont) ts
printTokens' kont (TkComment line ts) = do
    putStrLn $ showString "TkComment " . showsPrec 11 line $ ""
    printTokens' kont ts
printTokens' kont (TkEnd ts) = do
    putStrLn "TkEnd $"
    kont ts
printTokens' _kont (TkErr e) = do
    putStrLn $ showString "TkErr " . showsPrec 11 e $ ""
    
printTkFieldLines' :: (Show e, Show ann) => (k -> IO ()) -> TkFieldLines ann k e -> IO ()
printTkFieldLines' kont (TkFieldLine line ts) = do
    putStrLn $ showString "TkFieldLine " . showsPrec 11 line $ ""
    printTkFieldLines' kont ts
printTkFieldLines' kont (TkFieldComment line ts) = do
    putStrLn $ showString "TkFieldComment " . showsPrec 11 line $ ""
    printTkFieldLines' kont ts
printTkFieldLines' kont (TkFieldEnd ts) = do
    putStrLn "TkFieldEnd $"
    kont ts
printTkFieldLines' _kont (TkFieldErr e) = do
    putStrLn $ showString "TkFieldErr " . showsPrec 11 e $ ""
