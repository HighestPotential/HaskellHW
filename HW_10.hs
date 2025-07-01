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
                    makeUpper (x:xs) = toUpper x : makeUpper xs
            _ ->putStr content
        )

--c

--give al the time one subarray adn then at at the end unline
intoLines name [] = []
intoLines name content = unlines (intoWords name (lines content))

intoWords name [] = []
intoWords name (linewords : textInLines) = unwords
    (changeName name (words linewords)) : intoWords name textInLines

-- recives [ "hi", "all in one line"]
changeName name [] = []
changeName name (word:xs) | word == "Macarena" = name : changeName name xs
                            | otherwise         = word : changeName name xs




main3 :: IO ()
main3 = withFile "one-hit-wonder.txt" ReadMode (\ xs ->
    do
        putStrLn "Waehlen Sie einen Gesangsstil:"
        style <- getLine
        content<- hGetContents xs

        putStrLn "Wie ist Ihr Name?"
        name<- getLine

        let newNameContent = intoLines name content


        case style of
            "leise"->putStr (makeLower newNameContent)
                where
                    makeLower [] = ""
                    makeLower [x] = [toLower x]
                    makeLower (x:xs) = toLower x : (makeLower xs)
            "laut"-> putStr (makeUpper newNameContent)
                where
                    makeUpper [] = ""
                    makeUpper [x] = [toUpper x]
                    makeUpper (x:xs) = toUpper x : (makeUpper xs)
            _ ->putStr newNameContent
        )


--2
--a

main4 :: IO ()
main4 = do
    hSetBuffering stdout NoBuffering
    putStr "Deutsches Wort: "
    dWord <- getLine
    putStr "Bairisches Wort: "
    bWord <- getLine
    if bWord /="" && dWord /=""
        then do
            putStrLn ("'"++ dWord ++ "'" ++ " heisst auf Bairisch "
                ++ "'" ++ bWord ++"'")
            -- appendFile "woerterbuch.txt" ("(\"" ++ dWord 
            --     ++ "\"," ++ "\"" ++bWord ++ "\") \n")
            addToDict dWord bWord
            main4
    else
        do
            putStrLn "Auf wiedersehen!"

--b
addToDict :: [Char] -> [Char] -> IO ()
addToDict dword bword = withFile "woerterbuch.txt" AppendMode
    (\h -> do
        hPutStr h ("(\"" ++ dword ++ "\"," ++ "\"" ++bword ++ "\") \n")
        )

--3
--a
translator = withFile "woerterbuch.txt" ReadMode (\h ->
    do
        hSetBuffering stdout NoBuffering
        putStr "Ein hochdetusches Wort eingeben: "
        hdWord <- getLine
        dictContent <- hGetContents h
        let wordPairs = lines dictContent
        let typeWordPairs = toTuples wordPairs
        let possibleTranslationsStirngArray = dictLookup typeWordPairs hdWord
        let possibleTranslationsString = connect possibleTranslationsStirngArray

        hSetBuffering stdout LineBuffering
        putStr "Hier ist die Übersetzung: "
        putStrLn possibleTranslationsString
        )

connect []= ""
connect [x]= x
connect (x: xs) = x++ ", " ++connect xs

toTuples :: [String] -> [(String, String)]
toTuples [] = []
toTuples (x:xs)= read x: toTuples xs


dictLookup :: [(String, String)] -> String -> [String]
dictLookup (x:xs) word | word == fst x = snd x : dictLookup xs word
                        |otherwise = dictLookup xs word
dictLookup [] word = []


--b


translateSentence  = withFile "original.txt" ReadMode (\h ->
    do
        sentence <- hGetContents h
        mapTranslator (words sentence)
        putStrLn ""

    )

mapTranslator (x:xs) =
    do
        translator2 x
        mapTranslator xs
mapTranslator [] = translator2 ","

translator2 word = withFile "woerterbuch.txt" ReadMode (\h ->
    do
        if word == "," || word == "."
            then
                do
                putStr ""
            else
                do

                dictContent <- hGetContents h
                let wordPairs = lines dictContent
                let typeWordPairs = toTuples wordPairs
                let possibleTranslationsStirngArray = dictLookup typeWordPairs word
                let possibleTranslationsString = connect possibleTranslationsStirngArray
                if null possibleTranslationsString
                    then
                        do
                            putStr ""
                else
                    do
                    putStr (possibleTranslationsString ++ " ")
        )
