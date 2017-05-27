--meld takes two lists and returns a list contaning alternating elements from its argumets
-- e.g.:
-- meld [1,2,3] [4,5,6]
-- [1,4,2,5,3,6]
-- e.g.:
-- meld [1,2,3] [4,5,6,7,8,9,10,11]
-- [1,4,2,5,3,6,7,8,9,10,11]


meld :: [a] -> [a] -> [a]
meld xs [] = xs
meld [] ys = ys
meld (x:xs) (y:ys) = [x, y] ++ meld xs ys
