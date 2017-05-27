--This is for sort function
import Data.List

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeInteger :: [String] -> [Int]
makeInteger = map read

makeDouble :: [String] -> [Double]
makeDouble = map read

main = do
  content <- readLines "111.txt"
  print (sort content)
  print (sort (makeInteger content))
  print (sort (makeDouble content))

