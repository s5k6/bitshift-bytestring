module Data.ByteString.BitShift
  ( -- * bit-shifting entire ByteStrings
    bitShift
    -- * Lower level functions
  , bitsRight, bitsLeft, bytesRight, bytesLeft
  ) where

import qualified Data.ByteString as B
import Data.Bits ( shiftL, shiftR, (.|.) )



{-| This function provides arbitrary bit-shifts in either direction, by
adequately composing the other (partial) shifting functions in this
module.  @bitShift@ is a total function.

@bitShift n@ shifts a given `B.ByteString` by @n@ __bits__ to the left
(resp. right) iff @n@ is negative (resp. positive).

Bits and bytes are lost on the end shifted towards.  The other end of
the @ByteString@ is padded with zero bits and bytes.


=== Examples

  * Shift one bit to the right:

        >>> unpack . bitShift 1 $ pack [ 1, 0, 128 ]
        [0,128,64]

  * Shift three bits to the left:

        >>> unpack . bitShift (negate 3) $ pack [ 1, 0, 128 ]
        [8,4,0]

  * Shift two bytes to the left:

        >>> unpack . bitShift (negate 16) $ pack [ 1, 0, 128 ]
        [128,0,0]


=== Properties

prop> bitShift 0 = id

prop> signum a = signum b  =>  bitShift a . bitShift b = bitShift (a + b)

prop> l = length bs, abs n ≥ 8 * l  =>  bitShift n bs = replicate l 0
-}

bitShift :: Int -> B.ByteString -> B.ByteString

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



{-| @bitsRight n@ shifts a given `B.ByteString` by @n@ __bits__ to the
right.  I.e., it performs a __non-zero sub-byte__ shift to the right.
This needs to be combined with `bytesRight` for larger shifts.  The
result has the same length as the original @ByteString@, the first
byte padded with zero bits on the left.

/Partial:/ @n@ must be in the range 1 ≤ @n@ ≤ 7. -}

bitsRight :: Int -> B.ByteString -> B.ByteString

bitsRight n
  | 0 < n && n < 8 = bitsRight_ n
  | otherwise = error "bitsRight: argument out of range 1..7"



{-| Analogous to `bitsRight`, but shifts to the left.  In particular,
@n@ must be positive. -}

bitsLeft :: Int -> B.ByteString -> B.ByteString

bitsLeft n
  | 0 < n && n < 8 = bitsLeft_ n
  | otherwise = error "bitsLeft: argument out of range 1..7"



{-| @bytesRight m@ shifts a given `B.ByteString` by @m@ __bytes__ to the
right.  The result has the same length as the original @ByteString@,
zero-padded on the left.

/Partial:/ @m@ must be non-negative. -}

bytesRight :: Int -> B.ByteString -> B.ByteString

bytesRight m
  | m < 0 = error "bytesRight: negative argument"
  | m == 0 = id
  | otherwise = bytesRight_ id m



{-| Analogous to `bytesRight`, but shifts to the left.  In particular,
@m@ must be positive.-}

bytesLeft :: Int -> B.ByteString -> B.ByteString

bytesLeft m
  | m < 0 = error "bytesLeft: negative argument"
  | m == 0 = id
  | otherwise = bytesLeft_ id m



{- The following functions do not perform range checking on the
arguments.  They are intended for internal use. -}



bitsRight_ :: Int -> B.ByteString -> B.ByteString

bitsRight_ n = B.pack . go 0
  where
    go y bs = case B.uncons bs of
      Just (x, xs) -> ( shiftL y (8-n) .|. shiftR x n ) : go x xs
      Nothing -> []



bitsLeft_ :: Int -> B.ByteString -> B.ByteString

bitsLeft_ n = B.pack . maybe [] (uncurry go) . B.uncons
  where
    go y bs = case B.uncons bs of
      Just (x, xs) -> ( shiftL y n .|. shiftR x (8-n) ) : go x xs
      Nothing -> [shiftL y n]



{- It is not necessary to bit-shift the entire string when combined with
byte-shifting.  It is sufficient to bit-shift the part of the original
string that survives the shift, and then pad with zero.  To this end,
the byte-shifting fynctions `bytesLeft_` and `bytesRight_` take an
additional `B.ByteString -> B.ByteString` as first argument, which is
applied to the byte string between chopping on one side and padding on
the other. -}

bytesRight_
  :: (B.ByteString -> B.ByteString) -> Int -> B.ByteString -> B.ByteString

bytesRight_ f m bs
  | m < B.length bs = B.replicate m 0 <> f (B.dropEnd m bs)
  | otherwise = B.replicate (B.length bs) 0



bytesLeft_
  :: (B.ByteString -> B.ByteString) -> Int -> B.ByteString -> B.ByteString

bytesLeft_ f m bs
  | m < B.length bs = f (B.drop m bs) <> B.replicate m 0
  | otherwise = B.replicate (B.length bs) 0
