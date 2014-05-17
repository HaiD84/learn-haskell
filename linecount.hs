import Prelude hiding (catch)
{- This module have modern "try", "catch", "finally" functions: -}
import Control.Exception
import System.Environment
{- This module have outdated functions, so import only those we really need: -}
import System.IO.Error (isDoesNotExistError, ioeGetFileName)

countLines :: String -> IO ()
countLines fileName = do
    content <- readFile fileName
    putStrLn $ "В файле " ++ show (length (lines content)) ++ " строк"

handler :: IOException -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of
            Just fileName -> putStrLn $ "Файл " ++ fileName ++ " не существует!"
            Nothing -> putStrLn "Файл не существует!"
    | otherwise = ioError e

main = do
    (fileName:_) <- getArgs
    countLines fileName `catch` handler
