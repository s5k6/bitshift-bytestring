module Main ( main ) where

import qualified Data.ByteString as B
import Data.Bits ( shiftL, shiftR, (.|.), testBit )



bitsRight :: Int -> B.ByteString -> B.ByteString

bitsRight n
  | 0 < n && n < 8 = B.pack . go 0
  | otherwise = error "bitsRight: argument out of range 1..7"
  where
    go y bs = case B.uncons bs of
      Just (x, xs) -> ( shiftL y (8-n) .|. shiftR x n ) : go x xs
      Nothing -> []



bitsLeft :: Int -> B.ByteString -> B.ByteString

bitsLeft n
  | 0 < n && n < 8 = B.pack . maybe [] (uncurry go) . B.uncons
  | otherwise = error "bitsLeft: argument out of range 1..7"
  where
    go y bs = case B.uncons bs of
      Just (x, xs) -> ( shiftL y n .|. shiftR x (8-n) ) : go x xs
      Nothing -> [shiftL y n]



bytesRight :: Int -> B.ByteString -> B.ByteString

bytesRight m bs
  | m < 1 = error "bytesRight: negative argument"
  | m < B.length bs = B.replicate m 0 <> B.dropEnd m bs
  | otherwise = B.replicate (B.length bs) 0



bytesLeft :: Int -> B.ByteString -> B.ByteString

bytesLeft m bs
  | m < 1 = error "bytesLeft: negative argument"
  | m < B.length bs = B.drop m bs <> B.replicate m 0
  | otherwise = B.replicate (B.length bs) 0



shift :: Int -> B.ByteString -> B.ByteString

shift d | d < 0 = f (negate d) bitsLeft bytesLeft
        | d > 0 = f d bitsRight bytesRight
        | otherwise = id
  where
    f s bits bytes = case s `divMod` 8 of
                       (m,0) -> bytes m
                       (0,n) -> bits n
                       (m,n) -> bits n . bytes m






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
