module Data.ByteString.BitShift (
  bitsRight, bitsLeft, bytesRight, bytesLeft, shift
) where

import qualified Data.ByteString as B
import Data.Bits ( shiftL, shiftR, (.|.) )



bitsRight :: Int -> B.ByteString -> B.ByteString

bitsRight n
  | 0 < n && n < 8 = bitsRight_ n
  | otherwise = error "bitsRight: argument out of range 1..7"

bitsRight_ :: Int -> B.ByteString -> B.ByteString

bitsRight_ n = B.pack . go 0
  where
    go y bs = case B.uncons bs of
      Just (x, xs) -> ( shiftL y (8-n) .|. shiftR x n ) : go x xs
      Nothing -> []



bitsLeft :: Int -> B.ByteString -> B.ByteString

bitsLeft n
  | 0 < n && n < 8 = bitsLeft_ n
  | otherwise = error "bitsLeft: argument out of range 1..7"

bitsLeft_ :: Int -> B.ByteString -> B.ByteString

bitsLeft_ n = B.pack . maybe [] (uncurry go) . B.uncons
  where
    go y bs = case B.uncons bs of
      Just (x, xs) -> ( shiftL y n .|. shiftR x (8-n) ) : go x xs
      Nothing -> [shiftL y n]



bytesRight :: Int -> B.ByteString -> B.ByteString

bytesRight m
  | m < 1 = error "bytesRight: negative argument"
  | otherwise = bytesRight_ m

bytesRight_ :: Int -> B.ByteString -> B.ByteString

bytesRight_ m bs
  | m < B.length bs = B.replicate m 0 <> B.dropEnd m bs
  | otherwise = B.replicate (B.length bs) 0



bytesLeft :: Int -> B.ByteString -> B.ByteString

bytesLeft m
  | m < 1 = error "bytesLeft: negative argument"
  | otherwise = bytesLeft_ m

bytesLeft_ :: Int -> B.ByteString -> B.ByteString

bytesLeft_ m bs
  | m < B.length bs = B.drop m bs <> B.replicate m 0
  | otherwise = B.replicate (B.length bs) 0



shift :: Int -> B.ByteString -> B.ByteString

shift d
  | d < 0 = f (negate d) bitsLeft_ bytesLeft_
  | d > 0 = f d bitsRight_ bytesRight_
  | otherwise = id
  where
    f s bits bytes =
      case s `divMod` 8 of
        (m,0) -> bytes m
        (0,n) -> bits n
        (m,n) -> bits n . bytes m

    {- Note: This could be further optimised.  E.g., it is not necessary to
    bit-shift the entire string when combined with byte-shifting.  It
    would be sufficient to bit-shift the part of the original string
    that survives the shift, and then pad with zeros .-}
