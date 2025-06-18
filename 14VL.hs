import Data.Char

main :: IO ()
main = interact processInput
    where
        processInput = (map toUpper) . shortLines 

shortLines :: String -> String
shortLines = unlines . shortfilter . lines
    where
    shortfilter = filter (\line -> length line <= 10)

hello2 :: IO String
hello2 = do
    putStrLn "Ihr Name bitte: "
    name <- getLine
    _ <- putStrLn ("Sie heissen " ++ name ++ "?")
    getLine

hello :: IO ()
hello = do
    name <- getLine
    putStrLn ("Hello " ++ " ")


hello3 :: IO String
hello3 = do
    putStrLn "Ihr Name bitte: "
    name <- getLine
    putStrLn $ "Sie heissen " ++ name ++ "?" ++ sicher
    getLine
  where
    sicher = '\n' : "Sind Sie sich sicher?"


aktion1 :: IO ()
aktion1 = putStrLn "Hallo"

aktion2 :: IO ()
aktion2 = putStrLn "Welt"

main' :: IO ()
main' = aktion1 >> aktion2
