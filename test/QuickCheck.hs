module Main ( main ) where

import qualified Data.ByteString as B
import Data.Bits ( testBit, (.&.) )
import Data.ByteString.BitShift
import Test.QuickCheck hiding ( (.&.) )
import Data.List ( intersperse )
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import Data.Word ( Word8, Word64 )
import Test.QuickCheck.Monadic ( assertException )
import Control.Exception.Base ( ErrorCall )



binDump :: B.ByteString -> ShowS

binDump bs = foldl (.) id . intersperse (showChar '.') . map f $ B.unpack bs
  where
    f b = showString [ if testBit b i then '1' else '0' | i <- [7, 6 .. 0] ]


{- Expression (a <+- b) copies sign from (b) to (a), or leaves it
inchanged iff (b == 0).  I.e., (abs (a <+- b) == abs a), and (signum
(a <+- b) == signum b) -}

(<+-) :: Integral a => a -> a -> a

a <+- b = if a * b < 0 then negate a else a

prop_signCopy :: Int -> Int -> Bool

prop_signCopy a b =
  abs x == abs a
  &&
  signum x == signum (if a * b == 0 then a else b)
  where
    x = a <+- b



instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary


{- A SimpleCase is just a bytestring, but we extend it with its own Show
instance. -}

newtype SimpleCase = SimpleCase B.ByteString

instance Arbitrary SimpleCase where
  arbitrary = SimpleCase <$> arbitrary

instance Show SimpleCase where
  showsPrec _ (SimpleCase bs) = showString "Simple: " . binDump bs


{- A LimitedBitCase adds an integer specifying a bit shift, limited to
the length of the ByteString *in bits*.  This is to focus the selected
shift operations to those that do not zero the entire string.
Excessive shifting is tested separately.-}

data LimitedBitCase = LimitedBitCase Int B.ByteString

instance Arbitrary LimitedBitCase where
  arbitrary = do
    bs <- arbitrary
    let s = 8 * B.length bs                                -- NOTE, factor is 8
    LimitedBitCase <$> choose (negate s, s) <*> pure bs

instance Show LimitedBitCase where
  showsPrec _ (LimitedBitCase n bs) =
    showString "LimitedBits: " . showsLimitedCase n bs


showsLimitedCase :: Show a => a -> B.ByteString -> ShowS

showsLimitedCase n bs =
  shows n . showString ": " . binDump bs


{- A LimitedBitCase adds an integer specifying a byte shift, limited to
the length of the ByteString *in bytes*.  This is to focus the
selected shift operations to those that do not zero the entire string.
Excessive shifting is tested separately.-}

data LimitedByteCase = LimitedByteCase Int B.ByteString

instance Arbitrary LimitedByteCase where
  arbitrary = do
    bs <- arbitrary
    let s = B.length bs                                     -- NOTE, factor is 1
    LimitedByteCase <$> choose (negate s, s) <*> pure bs

instance Show LimitedByteCase where
  showsPrec _ (LimitedByteCase n bs) =
    showString "LimitedBytes: " . showsLimitedCase n bs



{- This is only available with QuickCheck >=2.15 -}

wantError :: a -> Property

wantError x = assertException isError x
  where
    isError :: ErrorCall -> Bool
    isError _ = True



{- Properties: Alignment with lower-level frontends. -}

prop_bitsLeft :: Int -> SimpleCase -> Bool

prop_bitsLeft n' (SimpleCase bs) =

  bitsLeft n bs == bitShift (negate n) bs

  where
    n = 1 + abs n' `mod` 7


prop_bitsLeftError :: Int -> B.ByteString -> Property

prop_bitsLeftError n bs =

  n < 1 || 7 < n ==> wantError ( bitsLeft n bs `seq` True )


prop_bitsRight :: Int -> SimpleCase -> Bool

prop_bitsRight n' (SimpleCase bs) =

  bitsRight n bs == bitShift n bs

  where
    n = 1 + abs n' `mod` 7


prop_bitsRightError :: Int -> B.ByteString -> Property

prop_bitsRightError n bs =

  n < 1 || 7 < n ==> wantError ( bitsRight n bs `seq` True )


prop_bytesLeft :: Int -> SimpleCase -> Bool

prop_bytesLeft n' (SimpleCase bs) =

  bytesLeft n bs == bitShift (8 * negate n) bs

  where
    n = abs n'


prop_bytesLeftError :: Int -> B.ByteString -> Property

prop_bytesLeftError n bs =

  n < 0 ==> wantError ( bytesLeft n bs `seq` True )


prop_bytesRight :: Int -> SimpleCase -> Bool

prop_bytesRight n' (SimpleCase bs) =

  bytesRight n bs == bitShift (8 * n) bs

  where
    n = abs n'


prop_bytesRightError :: Int -> B.ByteString -> Property

prop_bytesRightError n bs =

  n < 0 ==> wantError ( bytesRight n bs `seq` True )



{- Property: Shifting is length-invariant. -}

prop_lengthInvar :: LimitedBitCase -> Bool

prop_lengthInvar (LimitedBitCase n bs) =

  B.length bs == B.length (bitShift n bs)



{- Property: Not shifting is identity -}

prop_zeroShiftId :: SimpleCase -> Bool

prop_zeroShiftId (SimpleCase bs) =

  bitShift 0 bs == bs



{- Property: Excessive shifting is all-nought -}

prop_shiftOutAll :: Int -> SimpleCase -> Bool

prop_shiftOutAll e (SimpleCase bs) =

  bitShift s bs == B.replicate (B.length bs) 0

  where
    s = (e + 8 * (B.length bs <+- e))



{- Property: Shifting twice in the same direction is the same as
shifting once. -}

prop_shiftSum :: Int -> LimitedBitCase -> Bool

prop_shiftSum split (LimitedBitCase n bs) =

  a + b == n  -- self test: split n into two integers
  &&
  bitShift a (bitShift b bs) == bitShift (a + b) bs

  where
    a = if n == 0 then 0 else (abs split `mod` abs n) <+- n
    b = (abs n - abs a) <+- n



{- Property: Shifting by bytes is like shifting by 8 bits. -}

prop_byteIs8bits :: LimitedByteCase -> Bool

prop_byteIs8bits (LimitedByteCase n bs)

  | n < 0 = left
  | n > 0 = right
  | otherwise = left && right

  where

    left = bytesLeft (negate n) bs == bitShift (8 * n) bs

    right = bytesRight n bs == bitShift (8 * n) bs



{- Property: reversing commutes with shifting *bytes* modulo direction.
This is not true for bit-wise shifts, because the individual bits are
not turned around. -}

prop_bytesReverseShiftCommutes :: LimitedByteCase -> Bool

prop_bytesReverseShiftCommutes (LimitedByteCase n' bs) =

  (B.reverse . bytesRight n) bs == (bytesLeft n . B.reverse) bs

  where
    n = abs n'



{- Reverse bits in one byte.  Cf. Note 1 for reference. -}

twiddle :: Word8 -> Word8

twiddle b =
  fromIntegral
  $
  ((fromIntegral b * 0x0202020202) .&. 0x010884422010 :: Word64) `mod` 1023


{- Reverse ByteString bit by bit. -}

bitReverse :: B.ByteString -> B.ByteString

bitReverse = B.reverse . B.map twiddle

prop_bitReverse :: B.ByteString -> Bool
prop_bitReverse bs = (bitReverse . bitReverse) bs == bs


{- Property: reversing commutes with shifting modulo direction. -}

prop_bitsReverseShiftCommutes :: LimitedBitCase -> Bool

prop_bitsReverseShiftCommutes (LimitedBitCase n bs) =

  (bitReverse . bitShift n) bs == (bitShift (negate n) . bitReverse) bs



{- This is poor-man's rotation, relying on doubling the bytestring and
trimming it afterwards.  FIXME: add more efficient library function. -}

rotate :: Int -> B.ByteString -> B.ByteString

rotate n bs
  | n <= 8 * B.length bs = trim $ bitShift n (bs <> bs)
  | otherwise = error "rotate: too much"
  where
    l = B.length bs
    trim = if n < 0 then B.take l else B.takeEnd l



{- Property: Rotating forwards and backwards by the same amount yields
identity. -}

prop_rotateFwdBwdId :: LimitedBitCase -> Bool

prop_rotateFwdBwdId (LimitedBitCase n bs) =

  (rotate n . rotate (negate n)) bs == bs



{- Property: One full rotattion is identity. -}

prop_fullRotationId :: SimpleCase -> Bool

prop_fullRotationId (SimpleCase bs) =

  rotate (8 * B.length bs) bs == bs



return [] -- this is somehow needed for template haskell



main :: IO ()

main = do
  putStrLn ""

  args <- getArgs >>= \case
    [] -> pure myStdArgs
    ["quick"] -> pure myStdArgs{ maxSuccess = 10, maxSize = 10 }
    ["large"] -> pure myStdArgs{ maxSuccess = 5000, maxSize = 100000 }
    _ -> error "Valid arguments: (none), quick, large"

  $(forAllProperties) (quickCheckWithResult args {-. verbose-} ) >>= \case
    True -> pure ()
    False -> exitFailure

  where
    myStdArgs = stdArgs{ maxSuccess = 1000, maxSize = 10000 }



{- Notes

Note 1:

Take from [1], which says:

    unsigned char b; // reverse this (8-bit) byte
    b = (b * 0x0202020202ULL & 0x010884422010ULL) % 1023;

This method was attributed to Rich Schroeppel in the Programming Hacks
section of Beeler, M., Gosper, R. W., and Schroeppel, R. HAKMEM. MIT
AI Memo 239, Feb. 29, 1972.

[1]: http://graphics.stanford.edu/~seander/bithacks.html#ReverseByteWith64BitsDiv

-}
