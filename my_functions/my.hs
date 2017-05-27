--Implement Matlab diff function
diff :: Num a => [a] -> [a]
diff (_:[]) = []
diff (x:(y:ys)) = (y-x):(diff (y:ys))


--Implement a function that takes only even indices from a list
takeEven :: [a] -> [a]
takeEven xs = [x | (x, n) <- zip xs [0..], even n]


--Implement a function that tells whether an element belongs to a list or not
--Same as Prelude's 'elem'
belongsTo :: Eq a => a -> [a] -> Bool
belongsTo _ [] = False
belongsTo y (x:xs) = if x == y then True else belongsTo y xs


--Implement a function that takes a list of couples and returns a list
--contaninng only the first elements of the couples
first :: [(a,b)] -> [a]
first [] = []
first [(x,_)] = [x]
first ((x,_):ys) = x:first ys
--As an alternative, using Prelude function 'fst'
first' xs = map fst xs


--Implement a function equivalent to Prelude's (!!)
elementAt :: Int -> [a] -> a
elementAt 0 (x:xs) = x
elementAt n (x:xs) = elementAt (n-1) xs


--Implement a function that returns the elements of a list at given indexes
select :: [Int] -> [a] -> [a]
select _ [] = []
select [] _ = []
select (n:ns) xs = [x | (x, m) <- zip xs [0..], m == n] ++ select ns xs

--Implement a function that takes a list of numbers and returns a list
--of numbers obtained by summing two adjacent elements.
--E.g.: [1,2,3,4] -> [3, 5, 7]
sumCouples :: Num a => [a] -> [a]
sumCouples [] = []
sumCouples [x] = []
sumCouples (x:(y:ys)) = (x+y):sumCouples(y:ys)
