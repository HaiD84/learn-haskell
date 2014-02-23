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
