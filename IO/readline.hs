--This is for sort function
import Data.List

readLines :: FilePath -> IO [String]
--readLines = fmap lines . readFile
--equivalent without point-free notation
readLines file = fmap lines (readFile file)

makeInteger :: [String] -> [Int]
makeInteger = map read

makeDouble :: [String] -> [Double]
makeDouble = map read

main = do
  content <- readLines "111.txt"
  print (sort content)
  print (sort (makeInteger content))
  print (sort (makeDouble content))

  --Equivalent without using fmap
  allFile <- readFile "111.txt"
  let myLines = lines allFile
  print (sort myLines)
