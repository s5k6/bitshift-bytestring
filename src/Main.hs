module Main ( main ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Bits ( shiftL, shiftR, (.|.), testBit )
import Data.List ( intersperse )



bitsRight :: Int -> B.ByteString -> B.ByteString

bitsRight n
  | 0 < n && n < 8 = B.pack . go 0 . B.unpack
  | otherwise = error $ "argument out of range 1..7"
  where
    go y (x:xs) = ( shiftL y (8-n) .|. shiftR x n ) : go x xs
    go _ _ = []



bitsLeft :: Int -> B.ByteString -> B.ByteString

bitsLeft n
  | 0 < n && n < 8 = B.pack . start . B.unpack
  | otherwise = error $ "argument out of range 1..7"
  where
    start (x:xs) = go x xs
    start [] = []
    go y (x:xs) = ( shiftL y n .|. shiftR x (8-n) ) : go x xs
    go y [] = [shiftL y n]



bytesRight :: Int -> B.ByteString -> B.ByteString

bytesRight n bs = B.replicate n 0 <> B.dropEnd n bs



bytesLeft :: Int -> B.ByteString -> B.ByteString

bytesLeft n bs = B.drop n bs <> B.replicate n 0


display :: B.ByteString -> IO ()
display =
  putStrLn . foldr ($) "" . {-intersperse (showChar '.') .-} map f . B.unpack
  where
    f b = showString [ if testBit b i then '1' else '0' | i <- [7, 6 .. 0] ]


t0 = B.pack [8]

t1 = B.pack [255, 0, 183 , 0, 255]

main :: IO ()

main = do
  display t0
  display $ bitsRight 1 t0
  putStrLn ""

  display t1
  mapM_ display [ bitsRight n t1 | n <- [1 .. 7] ]
  putStrLn ""

  display t1
  mapM_ display [ bitsLeft n t1 | n <- [1 .. 7] ]
