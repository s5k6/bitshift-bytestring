module Main ( main ) where

import Data.ByteString ( ByteString, pack, unpack )
import Data.ByteString.BitShift
import Data.Bits ( testBit )
import Data.List ( intersperse )



binDump :: ByteString -> String

binDump bs =
  (foldl (.) id . intersperse (showChar '.') . map f $ unpack bs) ""
  where
    f b = showString [ if testBit b i then '1' else '0' | i <- [7, 6 .. 0] ]



t1 :: ByteString

t1 = pack      [   255  ,    1   ,   128  ,  255   ]

-- bit pattern: 11111111.00000001.10000000.11111111



showOneShift :: Int -> IO ()

showOneShift n = do
  putStr $ show n
  putStr "\t"
  putStrLn . binDump $ bitShift n t1



main :: IO ()

main = mapM_ showOneShift [-33 .. 33]
