import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception


dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                        [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        newTodoTasks = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, handle) -> do
            hClose handle
            removeFile tempName)
        (\(tempName, handle) -> do
            hPutStr handle newTodoTasks
            hClose handle
            removeFile fileName
            renameFile tempName fileName)

main = do
    (command:argList) <- getArgs
    dispatch command argList
