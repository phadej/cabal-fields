module CabalFields.Internal.Input where

import Data.Word (Word8)

import qualified Data.ByteString              as BS
import qualified Distribution.Parsec.Position as C

data Input = Input
    { inpBS  :: {-# UNPACK #-} !BS.ByteString
    , inpPos :: {-# UNPACK #-} !C.Position
    }
  deriving Show

inpNull :: Input -> Bool
inpNull = BS.null . inpBS

mkInput :: BS.ByteString -> Input
mkInput bs = Input bs (C.Position 1 1)

inpSkipSpace :: Input -> Input
inpSkipSpace (Input bs pos) = Input sfx (advancePos pos pfx) where
    (pfx, sfx) = BS.span (\w -> w == 0x20 || w == 0x09) bs

inpSpan :: (Word8 -> Bool) -> Input -> (BS.ByteString, Input)
inpSpan p (Input bs pos) = (pfx, Input sfx (advancePos pos pfx)) where
    (pfx, sfx) = BS.span p bs

advancePos :: C.Position -> BS.ByteString -> C.Position
advancePos = go
  where
    go pos@(C.Position r c) bs = case BS.uncons bs of
        Nothing -> pos
        Just (10, bs') -> go (C.Position (r + 1) 1) bs'
        Just (13, bs') -> case BS.uncons bs' of
            Just (10, bs'') -> go (C.Position (r + 1) 1) bs''
            _               -> go (C.Position (r + 1) 1) bs'
        Just (_ , bs') -> go (C.Position r (c + 1)) bs'
