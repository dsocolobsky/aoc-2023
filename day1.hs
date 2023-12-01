import Data.Char (isDigit)

main = do
   content <- readFile "day1.txt"
   numbers <- mapM processLine (lines content)
   putStrLn $ show $ sum numbers

parseLine :: String -> Int
parseLine [x]    = read [x,x]
parseLine (x:xs) = read [x, last xs]

processLine :: String -> IO Int
processLine line = do
    return $ parseLine $ filter isDigit line
