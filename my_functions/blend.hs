blend :: [a] -> [a] -> [a]
blend x [] = x
blend [] y = y
blend (x:xs) (y:ys) = [x, y] ++ blend xs ys


