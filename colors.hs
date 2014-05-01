import Control.Monad

main = do
    colors <- forM [1, 2, 3, 4] (\a -> do
        putStrLn $ "What color is " ++ show a ++ "?"
        c <- getLine
        return c)
    putStrLn "Colors for 1, 2, 3, 4 are:"
    mapM putStrLn colors
