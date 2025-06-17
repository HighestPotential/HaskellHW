main :: IO ()
main= do
    putStrLn "what is your name?"
    neme <- getLine
    putStrLn ("Hello " ++ neme)

hello2 :: IO String
hello2 = do
    () <- putStrLn "Ihr Name bitte: "
    name <- getLine
    _ <- putStrLn ("Sie heissen " ++ name ++ "?")
    getLine

hello :: IO ()
hello = do
    name <- getLine
    putStrLn ("Hello " ++ " ")
    
   
