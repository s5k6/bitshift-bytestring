module Main ( main ) where

import qualified Data.ByteString as B
import Data.Bits ( testBit )
import Data.ByteString.BitShift
import Test.QuickCheck
import Data.List ( intersperse )
import System.Environment ( getArgs )
import System.Exit ( exitFailure )



newtype Case = Case { unCase :: B.ByteString }

instance Arbitrary Case where
  arbitrary = sized $ \n -> Case . B.pack <$> resize n arbitrary

instance Show Case where
  showsPrec _ =
    foldl (.) id . intersperse (showChar '.') . map f . B.unpack . unCase
    where
      f b = showString [ if testBit b i then '1' else '0' | i <- [7, 6 .. 0] ]



{- Property: Shifting is length-invariant. -}

prop_lengthInvar :: Int -> Case -> Bool

prop_lengthInvar n (Case bs) = B.length bs == B.length (bitShift n bs)



{- Property: Shifting twice in the same direction is the same as
shifting once. -}

prop_shiftSum :: Int -> Int -> Case -> Bool

prop_shiftSum m n' (Case bs) =
  bitShift m (bitShift n bs) == bitShift (m + n) bs

  where
    n = (n' `rem` 64) * signum m * signum n'  -- same sign



{- Property: Shifting by bytes is like shifting by 8 bits. -}

prop_byteIs8bits :: Int -> Case -> Bool

prop_byteIs8bits n (Case bs)
  | n < 0 = left
  | n > 0 = right
  | otherwise = left && right
  where
    left = bytesLeft (negate n) bs == bitShift (8 * n) bs
    right = bytesRight n bs == bitShift (8 * n) bs



{- Property: reversing commutes with shifting *bytes* modulo direction.
This is not true for bit-wise shifts, because the individual bits are
not turned around. -}

prop_revShiftCommutes :: Int -> Case -> Bool

prop_revShiftCommutes n' (Case bs) =
  B.reverse (bytesRight n bs) == bytesLeft n (B.reverse bs)
  where
    n = abs n'



return [] -- this is somehow needed for template haskell



main :: IO ()

main = do
  putStrLn ""

  args <- getArgs >>= \case
    [] -> pure myStdArgs
    [a] -> pure myStdArgs{ maxSuccess = read a }
    [a, b] -> pure myStdArgs{ maxSuccess = read a, maxSize = read b }
    _ -> error "Invalid arguments"

  $(forAllProperties) (quickCheckWithResult args) >>= \case
    True -> pure ()
    False -> exitFailure

  where
    myStdArgs = stdArgs{ maxSuccess = 1000, maxSize = 10000 }
