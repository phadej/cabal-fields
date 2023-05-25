{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans () where

import Data.TreeDiff.Class (ToExpr)
import GHC.Generics        (Generic)

import qualified Distribution.Fields.Field    as C
import qualified Distribution.Parsec.Position as C

deriving instance Generic (C.Name ann)
deriving instance Generic (C.Field ann)
deriving instance Generic (C.FieldLine ann)
deriving instance Generic (C.SectionArg ann)

instance ToExpr C.Position
instance ToExpr ann => ToExpr (C.Name ann)
instance ToExpr ann => ToExpr (C.Field ann)
instance ToExpr ann => ToExpr (C.FieldLine ann)
instance ToExpr ann => ToExpr (C.SectionArg ann)
