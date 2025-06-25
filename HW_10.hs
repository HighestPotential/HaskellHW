import System.IO
import Data.Char

--Aufgabe 1
--a
main :: IO ()
main = withFile "one-hit-wonder.txt" ReadMode (\ xs ->
    do 
        content<- hGetContents xs
        putStr content)

--b
main2 :: IO ()
main2 = withFile "one-hit-wonder.txt" ReadMode (\ xs ->
    do 
        putStrLn "Waehlen Sie einen Gesangsstil:"
        style <- getLine
        content<- hGetContents xs

        case style of
            "leise"->putStr (makeLower content)
                where
                    makeLower [] = ""
                    makeLower [x] = [toLower x]
                    makeLower (x:xs) = toLower x : (makeLower xs)
            "laut"-> putStr (makeUpper content)
                where
                    makeUpper [] = ""
                    makeUpper [x] = [toUpper x]
                    makeUpper (x:xs) = toUpper x : (makeUpper xs)
            _ ->putStr content
        )
