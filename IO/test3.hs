select :: [Int] -> [a] -> [a]
--select [n] xs = [xs !! n]
--select ns xs = [xs !! (ns !! 0)]
--select ns xs = [snd tmp] where tmp = zip [0..] xs !! 1
select ns xs = map snd [e | e <- nmtd, elem (fst e) ns]
               where nmtd = zip [0..] xs

selectodd :: [a] -> [a]
selectodd xs = map snd [e | e <- nmtd, odd (fst e)]
                  where nmtd = zip [0..] xs

selectal :: [Int] -> [a] -> [a]
selectal ns xs = map (xs !!) ns

selectals :: [Int] -> [a] -> [a]
selectals ns xs = [xs !! n | n <- ns]

