main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes =
    unlines
    . map (\line -> if isPal line then "Палиндром" else "Не палиндром")
    . lines

isPal :: String -> Bool
isPal line = line == reverse line
