import Control.Monad.Writer

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
      tell ["Закончили: " ++ show a]
      return a
    | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)

{-
 - This is effective appending to list.
 -
 - *Main> mapM_ putStrLn $ snd $ runWriter (gcd' 80 44)
 - 80 mod 44 = 36
 - 44 mod 36 = 8
 - 36 mod 8 = 4
 - 8 mod 4 = 0
 - Закончили: 4
 -}

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
      tell ["Закончили: " ++ show a]
      return a
    | otherwise = do
      result <- gcdReverse b (a `mod` b)
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      return result

{-
 - And this one is not.
 -
 - *Main> mapM_ putStrLn $ snd $ runWriter (gcdReverse 80 44)
 - Закончили: 4
 - 8 mod 4 = 0
 - 36 mod 8 = 4
 - 44 mod 36 = 8
 - 80 mod 44 = 36
 -}


{-
 - Lists, effective in any type of work:
 -}
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

{-
 - *Main> fromDiffList (toDiffList [1..4] `mappend` toDiffList [1..3])
 - [1,2,3,4,1,2,3]
 -}

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
    | b == 0 = do
      tell (toDiffList ["Закончили: " ++ show a])
      return a
    | otherwise = do
      result <- gcdReverse' b (a `mod` b)
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
      return result

{-
 - *Main> mapM_ putStrLn . fromDiffList . snd $ runWriter (gcdReverse' 80 44)
 - Закончили: 4
 - 8 mod 4 = 0
 - 36 mod 8 = 4
 - 44 mod 36 = 8
 - 80 mod 44 = 36
 -}


{-
 - Compare speed of two approaches of adding to list:
 -
 - *Main> take 20 . fromDiffList . snd . runWriter $ finalCountDown 100000
 -
 - *Main> take 20 . snd . runWriter $ finalCountDown' 100000
 -}
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x - 1)
    tell (toDiffList [show x])

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = tell ["0"]
finalCountDown' x = do
    finalCountDown' (x - 1)
    tell [show x]
