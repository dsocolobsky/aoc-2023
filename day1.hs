{-# LANGUAGE OverloadedStrings #-}
import Data.Char (isDigit)
import Data.Text (Text, replace, pack, unpack)

main = do
   content <- readFile "day1.txt"
   numbers <- mapM processLine (lines content)
   putStrLn $ show $ sum numbers


replacements = [("oneight", "18"), ("threeight", "38"), ("fiveight", "58"), ("nineight", "98"),
   ("twone", "21"), ("sevenine", "79"), ("eightwo", "82"), ("eighthree", "83"),
   ("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"),
   ("seven", "7"), ("eight", "8"), ("nine", "9")]

replaceInLine :: Text -> Text
replaceInLine line = replaceInLineAux line replacements

replaceInLineAux :: Text -> [(Text, Text)] -> Text
replaceInLineAux line [] = line
replaceInLineAux line ((r1,r2):rs) = replaceInLineAux nline rs
   where nline = replace r1 r2 line

parseLine :: String -> Int
parseLine [x]    = read [x,x]
parseLine (x:xs) = read [x, last xs]

processLine :: String -> IO Int
processLine line = do
    return $ parseLine $ filter isDigit $ unpack $ replaceInLine $ pack line
