-- Reads from standard input radio stream as read from 8 bit logic analyzer,
-- discards Is,Qs and emits I,Q values as signed 16 bit
-- With respect to first version uses Lazy evaluation instead of interwining
-- IO with data manipulation.
-- Reads  _all_ data from the file in a buffer and then such buffer is
-- processed. Haskell laziness guarantees that in fact the data is not really
-- loaded in memory.


-- Converts a value to its 16 bit signed representation
toSigned16 :: Int -> Int
toSigned16 x = if x <= 0x7fff then x else (x - 0xffff - 1)

-- Reads the whole file and stores lines in a list of Strings
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

-- Removes m out of n elements from a list
chopmn :: Int -> Int -> [a] -> [a]
chopmn _ _ [] = []
chopmn m n xs = take m xs ++ chopmn m n (drop n xs)

-- Converts 8 bit value pairs to 16 bit values
from8to16 :: [Int] -> [Int]
from8to16 [] = []
from8to16 (x:y:ys) = (x*256 + y):from8to16 ys

main = do
     content <- readLines "data.txt"            --Read all data
     let iqStrings = chopmn 4 8 content         --Remove I/Q
     let iq8 = map read iqStrings :: [Int]      --Convert to Int
     let iq16 = map toSigned16 (from8to16 iq8)  --Convert to 16 bit
     mapM_ putStrLn $ map show $ iq16           --Print data
