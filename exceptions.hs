import Control.Exception
import System.Environment

printQuotients :: Integer -> Integer -> IO ()
printQuotients a b = do
    print $ a `div` b
    print $ b `div` a

params :: [String] -> (Integer, Integer)
params [a, b] = (read a, read b)

mainAction :: [String] -> IO ()
mainAction args = do
    let (a, b) = params args
    printQuotients a b

handleArith :: ArithException -> IO ()
handleArith _ = putStrLn "Деление на 0!"

handleArgs :: PatternMatchFail -> IO ()
handleArgs _ = putStrLn "Неверное число параметров командной строки!"

handleOther :: SomeException -> IO ()
handleOther e = putStrLn $ "Неизвестное исключение: " ++ show e

main = do
    args <- getArgs
    {- Способ поймать одно ожидаемое исключение (или все с SomeException) -}
    {-res <- try (mainAction args) :: IO (Either SomeException ())
    case res of
        Left e -> putStrLn $ "Какая-то ошибка: " ++ show e
        Right () -> putStrLn "OK"-}

    {- Ловит подходящие под обработчик исключения: -}
    mainAction args `catches` [
        Handler handleArith,
        Handler handleArgs,
        Handler handleOther]

    putStrLn "Конец"
