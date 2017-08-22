-- This simple program prints first N nautal numbers to standard output.
-- N is passed as first argument
-- 'show' is mapped on each element of the list as if we call it directly
-- on the list just one string is produced describing the full list.
-- We then mapM_ putStrln on the resulting strings list which sequences the
-- IO actions on each element of the list

-- This is for 'getArgs'
import System.Environment

main = do
     args <- getArgs
     let nLines = read $ head args :: Int
     mapM_ putStrLn $ map show $ take nLines [1..]
