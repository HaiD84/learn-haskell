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

