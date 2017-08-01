-- Implements chop48 function that chops a list taking the first four elements
-- and discarding the next four elements of the list sequentially, i.e.:
-- chop48 [1..20] = [1,2,3,4,9,10,11,12,17,18,19,20]

-- For 'chunksOf'
import Data.List.Split.Internals

-- Pattern matching implementation. Not easy to make it general
chop48 :: [a] -> [a]
chop48 (x:y:z:w:_:_:_:_:xs) = x:y:z:w:chop48 xs
chop48 xs = take 4 xs

-- More general implementation: takes m out of n elements
chopmn :: Int -> Int -> [a] -> [a]
chopmn _ _ [] = []
chopmn m n xs = take m (take n xs) ++ chopmn m n (drop n xs)


-- Function that takes a list and generates a list of lists of given length out of it.
-- Same as 'chunksOf'
-- Taken from https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"

-- Alternative implementation. Needs 'ChunkOf' in package Data.List.Split.Internals
-- or 'group' defined above.

-- Makes a list of lists of length n. Takes only first m elements of every
-- sub-list and finally concatenates the result together
chopmn' :: Int -> Int -> [a] -> [a]
chopmn' m n xs = concat (map (take m) (chunksOf n xs))

-- Using point-free style
chopmn'' :: Int -> Int -> [a] -> [a]
chopmn'' m n = concat . map (take m) . chunksOf n
