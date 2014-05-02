import System.IO

main = do
    withFile "readfile.hs" ReadMode (\handle -> do
        content <- hGetContents handle
        putStr content)
