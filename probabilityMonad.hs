import Data.Ratio

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ fmap (\(x, p) -> (f x, p)) xs
{-
 - *Main> fmap negate (Prob [(42, 3 % 4), (-8, 1 % 4)])
 - Prob {getProb = [(-42,3 % 4),(8,1 % 4)]}
 -}

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs


demoSituation :: Prob (Prob Char)
demoSituation = Prob [
    (Prob [('a', 1 % 2), ('b', 1 % 2)], 1 % 4),
    (Prob [('c', 1 % 2), ('d', 1 % 2)], 3 % 4)
    ]
{-
 - *Main> demoSituation
 - Prob {getProb = [(Prob {getProb = [('a',1 % 2),('b',1 % 2)]},1 % 4),(Prob {getProb = [('c',1 % 2),('d',1 % 2)]},3 % 4)]}
 - *Main> flatten demoSituation
 - Prob {getProb = [('a',1 % 8),('b',1 % 8),('c',3 % 8),('d',3 % 8)]}
 -}


instance Monad Prob where
    return x = Prob [(x, 1 % 1)]
    m >>= f = flatten $ fmap f m
    fail _ = Prob []


data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree :: Coin -> Prob Bool
flipThree state = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (== state) [a, b, c])
{-
 - All 3 coins are Tails: 9/40
 - *Main> getProb $ flipThree Tails
 - [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]
 -}

chanceOfThree :: Coin -> Rational
chanceOfThree state = foldr (\(_, p) acc -> acc + p) 0 allChances
    where allChances = filter (\(x, p) -> x == True) (getProb $ flipThree state)
{-
 - *Main> chanceOfThree Tails
 - 9 % 40
 -}
