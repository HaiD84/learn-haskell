myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b = EQ
    | a < b = LT
    | otherwise = GT


{- body mass ingex -}
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi < skinny = "You're thin"
    | bmi < normal = "You're OK"
    | bmi < fat = "You're fat"
    | otherwise = "You're very fat!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25, 30)

calcBmis :: [(Float, Float)] -> [Float]
calcBmis xs = [bmi weight height | (weight, height) <- xs]
    where bmi weight height = weight / height^2

calcBmis' xs = [bmi | (weight, height) <- xs, let bmi = weight / height^2]


{- this functions are equal: -}
head' :: [a] -> a
head' [] = error "no head for empty list"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of
    [] -> error "no head for empty list"
    (x:_) -> x


describeList :: [a] -> String
describeList x = "list " ++
    case x of
        [] -> "empty"
        [a] -> "has one element"
        as -> "has many elements"

{- or -}
describeList' :: [a] -> String
describeList' x = "list " ++ what x
    where
        what [] = "empty"
        what [a] = "has one element"
        what as = "has many elements"
