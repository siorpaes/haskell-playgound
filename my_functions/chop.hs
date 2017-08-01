-- Chops a list taking the first four elements and discarding the next four
-- elements sequentially
chop :: [a] -> [a]
chop (x:y:z:w:_:_:_:_:xs) = x:y:z:w:chop xs
chop xs = take 4 xs


--Alternative implementation

-- Needs this function that takes a list and generates a list of lists
-- of given length out of it. Taken from https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"

-- Makes a list of lists of length 8. Takes only first 4 elements of every
-- sub-list and finally concatenates the result together
chop' :: [a] -> [a]
chop' xs = concat (map (take 4) (group 8 xs))

-- Using point-free style
chop'' :: [a] -> [a]
chop'' = concat . map (take 4) . group 8
