quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [ a | a <- xs, a <= x]
        biggerSorted = quicksort [ a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

{- or in one line: -}
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort [ a | a <- xs, a <= x] ++ [x] ++ quicksort [ a | a <- xs, a > x]

quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) = quicksort (filter (<= x) xs) ++ [x] ++ quicksort (filter (> x) xs)
