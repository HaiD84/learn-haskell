import Data.List
import Data.Char

wordNums :: String -> [(String, Int)]
wordNums = map (\xs -> (head xs, length xs)) . group . sort . words
{-
*Main> wordNums "boom boom bang bang   boom"
[("bang",2),("boom",3)]
-}

isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
needle `isInfixOf'` haystack = any (needle `isPrefixOf`) (tails haystack)
{-
*Main> isInfixOf "world" "Hello, world!"
True
*Main> isInfixOf "world" "Hello, World!"
False
*Main> [4, 5] `isInfixOf'` [1..8]
True
-}


{- Caesar cipher -}
encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode offset msg = encode (negate offset) msg


{- find number with sum of digits equals to N -}
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]
{-
*Main> firstTo 40
Just 49999
-}
