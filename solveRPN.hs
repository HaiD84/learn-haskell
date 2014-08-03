import Control.Monad

{-
 - Вычисление обратной польской записи:
 - *Main> solveRPN "2 2 2 * +"
 - 6.0
 -}
solveRPN :: String -> Maybe Double
solveRPN str = do
    [result] <- foldM foldingFunction [] (words str)
    return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:xs) "+" = return ((y + x) : xs)
foldingFunction (x:y:xs) "-" = return ((y - x) : xs)
foldingFunction (x:y:xs) "*" = return ((y * x) : xs)
foldingFunction (x:y:xs) "/" = return ((y / x) : xs)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
