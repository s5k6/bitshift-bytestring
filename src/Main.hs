module Main ( main ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Bits ( shiftL, shiftR, (.|.), testBit )
import Data.List ( intersperse )



bitsRight :: Int -> B.ByteString -> B.ByteString

bitsRight n
  | 0 < n && n < 8 = B.pack . go 0 . B.unpack
  | otherwise = error "bitsRight: argument out of range 1..7"
  where
    go y (x:xs) = ( shiftL y (8-n) .|. shiftR x n ) : go x xs
    go _ _ = []



bitsLeft :: Int -> B.ByteString -> B.ByteString

bitsLeft n
  | 0 < n && n < 8 = B.pack . start . B.unpack
  | otherwise = error "bitsLeft: argument out of range 1..7"
  where
    start (x:xs) = go x xs
    start [] = []
    go y (x:xs) = ( shiftL y n .|. shiftR x (8-n) ) : go x xs
    go y [] = [shiftL y n]



bytesRight :: Int -> B.ByteString -> B.ByteString

bytesRight n bs
  | n < 1 = error "bytesRight: negative argument"
  | n < B.length bs = B.replicate n 0 <> B.dropEnd n bs
  | otherwise = B.replicate (B.length bs) 0



bytesLeft :: Int -> B.ByteString -> B.ByteString

bytesLeft n bs
  | n < 1 = error "bytesLeft: negative argument"
  | n < B.length bs = B.drop n bs <> B.replicate n 0
  | otherwise = B.replicate (B.length bs) 0



shift :: Int -> B.ByteString -> B.ByteString

shift n | n > 0 = case n `divMod` 8 of
                    (by,0) -> bytesRight by
                    (0,bi) -> bitsRight bi
                    (by,bi) -> bitsRight bi . bytesRight by

shift n | n < 0 = case negate n `divMod` 8 of
                    (by,0) -> bytesLeft by
                    (0,bi) -> bitsLeft bi
                    (by,bi) -> bitsLeft bi . bytesLeft by

shift 0 = id





display :: B.ByteString -> IO ()
display =
  putStrLn . foldr ($) "" . {-intersperse (showChar '.') .-} map f . B.unpack
  where
    f b = showString [ if testBit b i then '1' else '0' | i <- [7, 6 .. 0] ]


t0 = B.pack [8]

t1 = B.pack [255, 1, 128, 255]

main :: IO ()

main = do
  mapM_ display [ shift n t1 | n <- [-33 .. 33] ]
