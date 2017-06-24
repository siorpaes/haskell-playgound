--Implement a function that returns only odd elements of a given list
--1) Build a list of couples carrying indices of each element
--2) Filter out only couples which second element has odd index
--3) Take the element from the selected couples
selectodd :: [a] -> [a]
selectodd xs = map snd [e | e <- nmtd, odd (fst e)]
                  where nmtd = zip [0..] xs

--Implement a function that returns given indices of a list
--Use same technique as above. Note that this approach does not support
--repeated or non-ordered indexes
select :: [Int] -> [a] -> [a]
select ns xs = map snd [e | e <- nmtd, elem (fst e) ns]
               where nmtd = zip [0..] xs

--Alternative implementation
selectal :: [Int] -> [a] -> [a]
selectal ns xs = map (xs !!) ns

--Yet another alternative implementation
selectals :: [Int] -> [a] -> [a]
selectals ns xs = [xs !! n | n <- ns]
