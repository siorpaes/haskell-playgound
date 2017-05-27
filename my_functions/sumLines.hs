--Reads from standard input numbers from lines and sums them in couples
--e.g.: [1,2,3,4] => [3,7]

import Control.Monad
import Data.Char

main = forever $ do
     ls <- sequence [getLine, getLine]
     let ns = map read ls :: [Int]
     putStrLn $ show (head ns + last ns)

