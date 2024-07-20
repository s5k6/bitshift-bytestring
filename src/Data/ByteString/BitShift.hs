module Data.ByteString.BitShift
  ( bitsRight, bitsLeft, bytesRight, bytesLeft, I.bitShift
  ) where

{- The functions in this module mainly add argument range checks to some
of the internal functions.  The byte-wise shifting functions also hide
the implementation detail of an internal shifter, see `Internal` for
why. -}

import qualified Data.ByteString.BitShift.Internal as I



bitsRight :: Int -> I.ByteStringShifter

bitsRight n
  | 0 < n && n < 8 = I.bitsRight n
  | otherwise = error "bitsRight: argument out of range 1..7"



bitsLeft :: Int -> I.ByteStringShifter

bitsLeft n
  | 0 < n && n < 8 = I.bitsLeft n
  | otherwise = error "bitsLeft: argument out of range 1..7"



bytesRight :: Int -> I.ByteStringShifter

bytesRight m
  | m < 0 = error "bytesRight: negative argument"
  | m == 0 = id
  | otherwise = I.bytesRight id m



bytesLeft :: Int -> I.ByteStringShifter

bytesLeft m
  | m < 0 = error "bytesLeft: negative argument"
  | m == 0 = id
  | otherwise = I.bytesLeft id m
