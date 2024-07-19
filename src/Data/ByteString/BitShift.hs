module Data.ByteString.BitShift (
  bitsRight, bitsLeft, bytesRight, bytesLeft, bitShift
) where

import qualified Data.ByteString as B
import Data.Bits ( shiftL, shiftR, (.|.) )



type ByteStringShifter = B.ByteString -> B.ByteString



{- The functions with a trailing underscore do not perform range
checking on the arguments.  They are intended for internal use.

It is not necessary to bit-shift the entire string when combined with
byte-shifting.  It is sufficient to bit-shift the part of the original
string that survives the shift, and then pad with zero.  To this end,
the internal byte-shifting fynctions `bytesLeft_` and `bytesRight_`
take an additional `ByteStringShifter` as first argument, which is
applied to the byte string between chopping on one side and padding at
the other. -}



bitsRight :: Int -> ByteStringShifter

bitsRight n
  | 0 < n && n < 8 = bitsRight_ n
  | otherwise = error "bitsRight: argument out of range 1..7"

bitsRight_ :: Int -> ByteStringShifter

bitsRight_ n = B.pack . go 0
  where
    go y bs = case B.uncons bs of
      Just (x, xs) -> ( shiftL y (8-n) .|. shiftR x n ) : go x xs
      Nothing -> []



bitsLeft :: Int -> ByteStringShifter

bitsLeft n
  | 0 < n && n < 8 = bitsLeft_ n
  | otherwise = error "bitsLeft: argument out of range 1..7"

bitsLeft_ :: Int -> ByteStringShifter

bitsLeft_ n = B.pack . maybe [] (uncurry go) . B.uncons
  where
    go y bs = case B.uncons bs of
      Just (x, xs) -> ( shiftL y n .|. shiftR x (8-n) ) : go x xs
      Nothing -> [shiftL y n]



bytesRight :: Int -> ByteStringShifter

bytesRight m
  | m < 1 = error "bytesRight: negative argument"
  | otherwise = bytesRight_ id m

bytesRight_ :: ByteStringShifter -> Int -> ByteStringShifter

bytesRight_ f m bs
  | m < B.length bs = B.replicate m 0 <> f (B.dropEnd m bs)
  | otherwise = B.replicate (B.length bs) 0



bytesLeft :: Int -> ByteStringShifter

bytesLeft m
  | m < 1 = error "bytesLeft: negative argument"
  | otherwise = bytesLeft_ id m

bytesLeft_ :: ByteStringShifter -> Int -> ByteStringShifter

bytesLeft_ f m bs
  | m < B.length bs = f (B.drop m bs) <> B.replicate m 0
  | otherwise = B.replicate (B.length bs) 0



{- This function provides arbitrary bit-shifts in either direction, with
a negative argument shifting to the left, and a positive one shifting
to the right.  A zero-shift is identity.  `bitShift` adequately
composes the bit- and byte-wise shift functions from this module. -}

bitShift :: Int -> ByteStringShifter

bitShift d
  | d < 0 = f (negate d) bitsLeft_ bytesLeft_
  | d > 0 = f d bitsRight_ bytesRight_
  | otherwise = id
  where
    f s bits bytes =
      case s `divMod` 8 of
        (m,0) -> bytes id m
        (0,n) -> bits n
        (m,n) -> bytes (bits n) m
