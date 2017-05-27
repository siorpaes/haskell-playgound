-- https://wiki.haskell.org/99_questions/

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





