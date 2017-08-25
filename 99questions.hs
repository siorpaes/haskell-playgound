-- https://wiki.haskell.org/99_questions

--Problem 1
mylast :: [a] -> a
mylast [x] = x
mylast (x:xs) = mylast xs

mylast' :: [a] -> a
mylast' xs = xs !! (length xs - 1)

mylast'' :: [a] -> a
mylast'' xs = head (drop ((length xs) - 1) xs)


--Problem 2
myButLast :: [a] -> a
myButLast xs = xs !! (length xs - 2)

myButLast' :: [a] -> a
myButLast' xs = head (drop ((length xs) - 2) xs)

--Problem 3
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n - 1)

--Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' xs = foldr (\_ n -> n + 1) 0 xs

--Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

--Problem 8
compress :: Eq a => [a] -> [a]
compress xs = foldr f [] xs
              where f x ys | ys == []     = [x]
                           | x /= head ys = x:ys
                           | otherwise    = ys

compress' :: Eq a => [a] -> [a]
compress' [x] = [x]
compress' xs = init $ foldr (\x ys -> if x /= head ys then x:ys else ys) [head xs] xs

--Problem 9
pack :: Eq a => [a] -> [[a]]
pack xs = foldr f [[]] xs
          where f x ys | ys == [[]]          = [[x]]
                       | x == head (head ys) = (x:head ys) : tail ys
                       | otherwise           = [x]:ys

--Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = zip (map length ys) (map head ys)
                where ys = pack xs

--Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

--Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (take n $ repeat x) ++ repli xs n

--Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery _ 0 = []
dropEvery xs n = (take (n-1) xs) ++ dropEvery (drop n xs) n
