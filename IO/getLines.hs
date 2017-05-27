--This example shows how to recursively define IO actions
--See "Programming in Haskell", section 9.5
--getLines function retrieves all available lines from standard input and puts
--them in a list of strings
--
-- Recall:
-- type IO a = World -> (a, World)


getLines :: IO [String]
getLines = do
           x <- getLine
           if null x then
             return []
           else
             do
             xs <- getLines
             return (x:xs)


main = do
       lines <- getLines
       putStrLn $ "We read " ++ (show $ length lines) ++ " lines"
       putStrLn $ unwords lines
