module Main ( main ) where

import qualified Data.ByteString as B
import Data.Bits ( testBit )
import Data.ByteString.BitShift



display :: B.ByteString -> IO ()

display =
  putStrLn . foldr ($) "" . {-intersperse (showChar '.') .-} map f . B.unpack
  where
    f b = showString [ if testBit b i then '1' else '0' | i <- [7, 6 .. 0] ]



t1 :: B.ByteString

t1 = B.pack [255, 1, 128, 255]



main :: IO ()

main = do
  putStrLn ""
  mapM_ display [ bitShift n t1 | n <- [-33 .. 33] ]

  putStrLn "WARNING: Not a test yet, just a demo"
