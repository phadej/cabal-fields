{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CabalFields.Compat (
    CF.readFields,
    CF.readFields',
    CF.LexWarning (..),
    CF.LexWarningType (..),
) where

import qualified Distribution.Fields.LexerMonad as CF
import qualified Distribution.Fields.Parser     as CF
import           GHC.Generics                   (Generic)

deriving instance Generic CF.LexWarningType
deriving instance Enum CF.LexWarningType
deriving instance Bounded CF.LexWarningType
