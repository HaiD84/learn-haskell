main = do
    putStrLn "¡Hola! ¿Como te llamas?"
    name <- getLine
    putStrLn $ "¡Hola, " ++ name ++ "! ¿Como estás?"
