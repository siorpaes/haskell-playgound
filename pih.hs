-- Exercise 2.6.3
-- 1) n cannot start with capital letter
-- 2) statements in 'where' clause must be column aligned
-- 3) div must be in back quotes, not forward ones. Use (ALT + 96) or switch to US keyboard
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- Exercise 2.6.4
last' :: [a] -> a
last' xs = head (drop (length xs - 1) xs)

--Alternative implementation with pattern matching
last'' :: [a] -> a
last'' [x] = x
last'' (x:xs) = last xs


--Exercise 2.6.5
init' ::  [a] -> [a]
init' xs = take (length xs - 1) xs

--Alternative implementation with pattern matching
init'' :: [a] -> [a]
init'' [] = []
init'' [x] = []
init'' (x:xs) = x:init'' xs


--Exercise 4.8.1
halve:: [a] -> ([a], [a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

--Exercise 4.8.2
safetail_a:: [a] -> [a]
safetail_a xs = if null xs then [] else tail xs

safetail_b:: [a] -> [a]
safetail_b xs | null xs = []
              | otherwise = tail xs

safetail_c:: [a] -> [a]
safetail_c [] = []
safetail_c xs = tail xs


--Exercise 4.8.6
mult:: Num a => a -> a -> a -> a
--mult x y z = x*y*z
mult = \x -> (\y -> (\z -> x*y*z))


--Exercise 5.7.1
s = sum [ x^2 | x <- [1..100] ]


--Exercise 5.7.2
length' xs = sum[1 | _ <- xs]

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n] ]

--Exercise 5.7.3
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2 == z^2]

--Exercise 5.7.4
factors:: Int -> [Int]
factors n = [x | x<-[2..n], n `mod` x == 0]

--This version of factors excludes the number itself from the list but includes '1' 
factorsx:: Int -> [Int]
factorsx n = [x | x<-[1..n-1], n `mod` x == 0]

perfects:: Int -> [Int]
perfects n = [x | x<-[1..n], x == (sum (factorsx x))]

--Exercise 5.7.7
scalar::  [Int] -> [Int] -> Int
scalar xs ys = sum [x * y | (x, y) <- zip xs ys]


--Exercise 6.8.1 Note that operators cannot have whatever name. E.g.: ^' is not allowed.
--So, we call our new operator (^!)
--Also note that (n+k) pattern is not longer supported since Haskell 2010
(^!) :: Int -> Int -> Int
_ ^! 0 = 1
n ^! m = n * (n ^! (m-1))

--Exercise 6.8.3
and' :: [Bool] -> Bool
and' [] = False
and' [x] = x
and' (x:xs) = x && (and xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' [xs] = xs
concat' (xs:xss) = xs ++ concat' xss

replicate'' :: Int -> a -> [a]
replicate'' 0 _ = []
replicate'' 1 x = [x]
replicate'' n x = (x : replicate'' (n-1) x)

(!!!) :: [a] -> Int -> a
[x] !!! 0 = x
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x [y] = if x == y then True else False
elem' x (y:ys) = if x == y then True else elem x ys

--Exercise 6.8.6
sum' :: Num a => [a] -> a
sum' [] = 0
sum' [x] = x
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' 1 (x:xs) = [x]
take' n (x:xs) = (x:take' (n-1) xs)

last''' :: [a] -> a
last''' [x] = x
last''' (x:xs) = last''' xs

--Exercise 7.8.1
--map f filter p xs

--Exercise 7.8.2
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr (&&) True (map f xs)

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldr (||) False (map f xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []                 = []
takeWhile' p (x:xs) | p x       = x:takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []                 = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs
