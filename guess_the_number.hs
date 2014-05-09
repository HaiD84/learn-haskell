import System.Random
import Control.Monad (when)

main = do
    {-gen <- getStdGen-}
    {- better use newStdGen to get new generator every time
     - you restart main program in ghci -}
    gen <- newStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStrLn "Я задумал число от 1 до 10. Какое? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if number == randNumber then
            putStrLn "Правильно!"
        else
            putStrLn $ "Извините, но правильный ответ: " ++ show randNumber
        askForNumber newGen
