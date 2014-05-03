import System.IO
import System.Directory
import Data.List
import Control.Exception

main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                        [0..] todoTasks
    putStrLn "Список дел:"
    mapM_ putStrLn numberedTasks
    putStrLn "Выберите номер для удаления:"
    deleteLine <- getLine
    let number = read deleteLine
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, handle) -> do
            hClose handle
            removeFile tempName)
        (\(tempName, handle) -> do
            hPutStr handle newTodoItems
            hClose handle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")
