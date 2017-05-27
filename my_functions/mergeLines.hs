--Reads from standard input lines and merges them two at the time

import Control.Monad
import Data.Char

main = forever $ do
     l1 <- getLine
     l2 <- getLine
     putStrLn (l1 ++ l2)

