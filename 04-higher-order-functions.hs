applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
{-
*Main> applyTwice (+3) 10
16
*Main> applyTwice (++ ", knock") "Hey"
"Hey, knock, knock"
*Main> applyTwice ("uno " ++) "dos"
"uno uno dos"
*Main> applyTwice (3:) [1]
[3,3,1]
-}


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
{-
*Main> zipWith' (+) [1..6] [1..10]
[2,4,6,8,10,12]
*Main> zipWith' max [6, 5, 1, 7] [ 3, 8, 6, 1]
[6,8,6,7]
*Main> zipWith' (zipWith' (*)) [[1..3], [4..6], [7..9]] [[3,2,1], [6,5,4], [9,8,7]]
[[3,4,3],[24,25,24],[63,64,63]]
-}


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
{-
*Main> map' (+3) [1..4]
[4,5,6,7]
*Main> map' (++ "!") ["boom", "bang"]
["boom!","bang!"]
*Main> map' (replicate 3) [1..3]
[[1,1,1],[2,2,2],[3,3,3]]
*Main> map' (map' (^2)) [[1,2], [3..6], [7..9]]
[[1,4],[9,16,25,36],[49,64,81]]
*Main> map' fst [(1, 2), (5, 4)]
[1,5]
-}


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs
{-
*Main> filter' (>3) [1..6]
[4,5,6]
*Main> filter' even [1..10]
[2,4,6,8,10]
*Main> let notNull x = not (null x) in filter' notNull [[], [1], [1..6], []]
[[1],[1,2,3,4,5,6]]
-}


{-
Prelude> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
166650
OR:
Prelude> sum (takeWhile (<10000) [x | x <- [n^2 | n <- [1..]], odd x])
166650
-}


{- Collatz (Syracuse) sequence -}
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz x
    | even x = x : collatz (x `div` 2)
    | odd x = x : collatz (3*x + 1)

{- how many Collatz sequences longer than 15 are between 1..100? -}
numLongChains :: Int
numLongChains = length (filter isLonger (map collatz [1..100]))
    where isLonger xs = length xs > 15

{- or with lambda function: -}
numLongChains' = length (filter (\xs -> length xs > 15) (map collatz [1..100]))


{- Lambdas without brackets goes to the end of the line.
 - Next two functions are equal.
 - (It is because all functions in Haskell are currying) -}
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

{- Does "flip" function reads better that way?
 - That record shows that mostly flip is used to create new functions. -}
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

{-
*Main> zipWith (flip (++)) ["love you", "love me"] ["I ", "you "]
["I love you","you love me"]
*Main> map (flip subtract 20) [1..4]
[19,18,17,16]
-}


{- left fold: -}
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

{- or knowing that functions are currying: -}
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

{- right fold: -}
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs


{- exercises. folds can be used if you need to calculate something in one pass through the list -}
drop' :: Int -> [a] -> [a]
drop' n xs = foldr (\x acc -> if length xs - length acc > n then x : acc else acc) [] xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f xs = foldl (\acc x -> if f x && length acc == 0 then [] else acc ++ [x]) [] xs


{- foldl1, foldr1: takes first element of a list as an initial value -}
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x : acc) [] xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

last' :: [a] -> a
last' xs = foldl1 (\_ x -> x) xs
{- same: -}
{-last' xs = foldr1 (\_ x -> x) xs-}


{- how many natural numbers do we need so sum of their sqrts' is above 1000? -}
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..])) ) + 1
