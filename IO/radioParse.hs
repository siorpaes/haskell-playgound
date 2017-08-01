-- Reads from standard input ARPU radio stream as read from 8 bit logic
-- analyzer. Discards Is,Qs and emits I,Q values as signed 16 bit.
-- Can work with hexadecimal representation

import Control.Monad
import Data.Char
import Text.Printf
import System.Environment   
import Data.List

toSigned16 :: Int -> Int
toSigned16 x = if x <= 0x7fff then x else (x - 0xffff - 1)

main = forever $ do
     args <- getArgs
     lines <- sequence [getLine, getLine, getLine, getLine]
     _     <- sequence [getLine, getLine, getLine, getLine]
     let ns = map read lines :: [Int]
     
     if null args then do
         print $ toSigned16 (ns !! 0 + 0x100 * ns !! 1)
         print $ toSigned16 (ns !! 2 + 0x100 * ns !! 3)
     else do
         putStrLn $ printf "%04x" (ns !! 0 + 0x100 * ns !! 1)
         putStrLn $ printf "%04x" (ns !! 2 + 0x100 * ns !! 3)


