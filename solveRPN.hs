{-
 - Вычисление обратной польской записи:
 - *Main> solveRPN "2 2 2 * +"
 - 6.0
 -}
solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
    where
        foldingFunction (x:y:xs) "+" = (y + x) : xs
        foldingFunction (x:y:xs) "-" = (y - x) : xs
        foldingFunction (x:y:xs) "*" = (y * x) : xs
        foldingFunction (x:y:xs) "/" = (y / x) : xs
        foldingFunction xs numberString = read numberString : xs
