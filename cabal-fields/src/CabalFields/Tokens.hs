{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalFields.Tokens (
    AnnByteString (..),
    Tokens (..),
    TkFieldLines (..),
) where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.ByteString    (ByteString)
import Data.Traversable   (fmapDefault, foldMapDefault)

data AnnByteString ann = ABS ann {-# UNPACK #-} !ByteString
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Tokens ann k e
    = TkSection !(AnnByteString ann) !(AnnByteString ann) (Tokens ann (Tokens ann k e) e)
    | TkField !(AnnByteString ann) !ann (TkFieldLines ann (Tokens ann k e) e)
    | TkComment !(AnnByteString ann) (Tokens ann k e)
    | TkEnd k
    | TkErr e
  deriving (Show, Eq)

instance Functor (Tokens ann k) where
    fmap = fmapDefault

instance Foldable (Tokens ann k) where
    foldMap = foldMapDefault

instance Traversable (Tokens ann k) where
    traverse = bitraverse pure

instance Bifunctor (Tokens ann) where
    bimap = bimapDefault

instance Bifoldable (Tokens ann) where
    bifoldMap = bifoldMapDefault

instance Bitraversable (Tokens ann) where
    bitraverse _ g (TkErr e) = TkErr <$> g e
    bitraverse f _ (TkEnd k) = TkEnd <$> f k
    bitraverse f g (TkComment l ts) = TkComment l <$> bitraverse f g ts
    bitraverse f g (TkField n c fls) = TkField n c <$> bitraverse (bitraverse f g) g fls
    bitraverse f g (TkSection n a ts) = TkSection n a <$> bitraverse (bitraverse f g) g ts

data TkFieldLines ann k e
    = TkFieldLine !(AnnByteString ann) (TkFieldLines ann k e)
    | TkFieldComment !(AnnByteString ann) (TkFieldLines ann k e)
    | TkFieldEnd k
    | TkFieldErr e
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Bifunctor (TkFieldLines ann) where
    bimap = bimapDefault

instance Bifoldable (TkFieldLines ann) where
    bifoldMap = bifoldMapDefault

instance Bitraversable (TkFieldLines ann) where
    bitraverse _ g (TkFieldErr e) = TkFieldErr <$> g e
    bitraverse f _ (TkFieldEnd k) = TkFieldEnd <$> f k
    bitraverse f g (TkFieldComment l fls) = TkFieldComment l <$> bitraverse f g fls
    bitraverse f g (TkFieldLine l fls) = TkFieldLine l <$> bitraverse f g fls
