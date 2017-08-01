-- Reads from standard input radio stream as read from 8 bit logic analyzer,
-- discards Is,Qs and emits I,Q values as signed 16 bit

import Control.Monad
import Data.Char

toSigned16 :: Int -> Int
toSigned16 x = if x <= 0x7fff then x else (x - 0xffff - 1)

main = forever $ do
     --Pick up I, Q
     ls <- sequence [getLine, getLine, getLine, getLine]

     --Discard Is, Qs
     _  <- sequence [getLine, getLine, getLine, getLine]

     let ns = map read ls :: [Int]

     --Convert to signed 16
     print $ toSigned16 (ns !! 0 + 256 * ns !! 1)
     print $ toSigned16 (ns !! 2 + 256 * ns !! 3)
