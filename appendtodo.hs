main = do
    line <- getLine
    appendFile "todo.txt" (line ++ "\n")
