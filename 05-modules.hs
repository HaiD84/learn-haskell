import Data.List
import Data.Char
import qualified Data.Map as Map

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


phonebook :: Map.Map String String
phonebook = Map.fromList
    [ ("Kate", "555-11-22")
    , ("Alice", "123-44-12")
    , ("Rose", "246-55-77")
    ]
{-
 - *Main> Map.lookup "Rose" phonebook
 - Just "246-55-77"
 - *Main> Map.lookup "Lena" phonebook
 - Nothing
 -
 - *Main> let newBook = Map.insert "Lena" "222-11-12" phonebook
 - *Main> newBook
 - fromList [("Alice","123-44-12"),("Kate","555-11-22"),("Lena","222-11-12"),("Rose","246-55-77")]
 - *Main> Map.lookup "Lena" newBook
 - Just "222-11-12"
 -
 - *Main> Map.size phonebook
 - 3
 - *Main> Map.size newBook
 - 4
 -}

{- Convert any string number to list of digits.
 - Data.Char is required.
 -}
string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit
{-
 - *Main> let intBook = Map.map string2digits phonebook
 - *Main> Map.lookup "Kate" intBook
 - Just [5,5,5,1,1,2,2]
 -}


listBook = [
     ("Kate", "555-11-22"),
     ("Kate", "555-11-44"),
     ("Alice", "123-44-12"),
     ("Rose", "246-55-77"),
     ("Rose", "246-55-71"),
     ("Rose", "246-00-77")
     ]
{- If there are duplicates in list, Map.fromList will throw them away:
 - *Main> Map.fromList listBook
 - fromList [("Alice","123-44-12"),("Kate","555-11-44"),("Rose","246-00-77")]
 -}

{- Join duplicates with ", ": -}
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2
{- *Main> phoneBookToMap listBook
 - fromList [("Alice","123-44-12"),("Kate","555-11-44, 555-11-22"),("Rose","246-00-77, 246-55-71, 246-55-77")]
 - *Main> Map.lookup "Kate" $ phoneBookToMap listBook
 - Just "555-11-44, 555-11-22"
 -}

{- Or create list of phones and then concatenate it: -}
phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
{-
 - *Main> Map.lookup "Kate" $ phoneBookToMap' listBook
 - Just ["555-11-44","555-11-22"]
 -}
