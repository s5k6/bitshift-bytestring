module Data.ByteString.BitShift.Internal
  ( ByteStringShifter
  , bitsRight, bitsLeft, bytesRight, bytesLeft
  ) where

{- The functions in this module do not perform range checking on the
arguments.  They are intended for internal use. -}

import qualified Data.ByteString as B
import Data.Bits ( shiftL, shiftR, (.|.) )



type ByteStringShifter = B.ByteString -> B.ByteString



bitsRight :: Int -> ByteStringShifter

bitsRight n = B.pack . go 0
  where
    go y bs = case B.uncons bs of
      Just (x, xs) -> ( shiftL y (8-n) .|. shiftR x n ) : go x xs
      Nothing -> []



bitsLeft :: Int -> ByteStringShifter

bitsLeft n = B.pack . maybe [] (uncurry go) . B.uncons
  where
    go y bs = case B.uncons bs of
      Just (x, xs) -> ( shiftL y n .|. shiftR x (8-n) ) : go x xs
      Nothing -> [shiftL y n]



{- It is not necessary to bit-shift the entire string when combined with
byte-shifting.  It is sufficient to bit-shift the part of the original
string that survives the shift, and then pad with zero.  To this end,
the byte-shifting fynctions `bytesLeft` and `bytesRight` take an
additional `ByteStringShifter` as first argument, which is applied to
the byte string between chopping on one side and padding on the other. -}


bytesRight :: ByteStringShifter -> Int -> ByteStringShifter

bytesRight f m bs
  | m < B.length bs = B.replicate m 0 <> f (B.dropEnd m bs)
  | otherwise = B.replicate (B.length bs) 0



bytesLeft :: ByteStringShifter -> Int -> ByteStringShifter

bytesLeft f m bs
  | m < B.length bs = f (B.drop m bs) <> B.replicate m 0
  | otherwise = B.replicate (B.length bs) 0
