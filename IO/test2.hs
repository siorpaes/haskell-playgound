import System.IO
main = do
     putStr "Hello, what is your name ? "
     hFlush stdout
     name <- getLine
     putStrLn ("Hey " ++ name ++ ", you rock!!")
