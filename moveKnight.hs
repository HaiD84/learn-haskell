import Control.Monad


type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [
        (c - 2, r - 1), (c - 2, r + 1),
        (c - 1, r + 2), (c + 1, r + 2),
        (c + 2, r + 1), (c + 2, r - 1),
        (c - 1, r - 2), (c + 1, r - 2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3Moves :: KnightPos -> [KnightPos]
in3Moves start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3Moves :: KnightPos -> KnightPos -> Bool
canReachIn3Moves start end = end `elem` in3Moves start

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start
