{-
 - These goes in ghci command line:
 - Prelude> 42 + 6
 -
 - Prelude> True && False
 - Prelude> True || False
 - Prelude> not False
 - Prelude> not False
 -
 - Prelude> 6 == 6
 - Prelude> 5 /= 6
 - Prelude> 1 <= 6
 - Prelude> "Hello" == "World"
 -
 - increment:
 - Prelude> succ 8
 - 9
 - functions has higher priority:
 - Prelude> succ 9*10
 - 100
 - Prelude> succ (9*10)
 - 91
 -
 - Prelude> div 92 10
 - 9
 - Prelude> 92 `div` 10
 - 9
 -}


{-
 - Functions:
 -}

doubleMe x = x + x
{-
 - Prelude> doubleMe 2
 - 4
 - Prelude> doubleMe (doubleMe 2)
 - 8
 -}
-- doubleUs x y = 2 * x + 2 * y
doubleUs x y = doubleMe x + doubleMe y
{-
 - Prelude> doubleUs 1 2
 - 6
 - Prelude> doubleUs 1 (doubleMe 1)
 - 6
 -}

doubleSmallNumber x = if x > 100 then x else x * 2
{-
 - Prelude> doubleSmallNumber 60
 - 120
 - Prelude> doubleSmallNumber 101
 - 101
 -}


{-
 - Lists:
 -}

{- You should use "let lostNumbers" in ghci -}
lostNumbers = [4, 8, 15, 16, 23, 42]
{-
 - Prelude> lostNumbers ++ lostNumbers
 - [4, 8, 15, 16, 23, 42, 4, 8, 15, 16, 23, 42]
 - Prelude> "Hello, " ++ "world!"
 - "Hello, world!"
 - Prelude> ['w', 'o', 'r'] ++ ['l', 'd']
 - "world"
 -
 - Prelude> [1, 2, 3, 4] ++ [5]
 - [1, 2, 3, 4, 5]
 - But be carefull with ++ on a big lists. Haskell has to walk through all first list to add something to the end.
 - Faster is to add something to the beginning of the list:
 - Prelude> 1 : [2, 3, 4, 5]
 - [1, 2, 3, 4, 5]
 -
 - Prelude> 1:2:3:[]
 - [1, 2, 3]
 -
 - Get element of the list by index:
 - Prelude> lostNumbers !! 1
 - 8
 - Prelude> "HaiD" !! 0
 - 'H'
 -
 - Prelude> [[1, 2], [2, 2]] ++ [[3, 3, 3]]
 - [[1,2],[2,2],[3,3,3]]
 -
 - Prelude> [1, 2, 3] > [1, 2, 2]
 - True
 - Prelude> [1, 2, 3] <= [1, 2]
 - False
 -
 - Prelude> head lostNumbers
 - 4
 - Prelude> tail lostNumbers
 - [8, 15, 16, 23, 42]
 - Prelude> init lostNumbers
 - [4, 8, 15, 16, 23]
 - Prelude> last lostNumbers
 - 42
 - Prelude> reverse lostNumbers
 - [42, 23, 16, 15, 8, 4]
 -
 - Prelude> null []
 - True
 - Prelude> null lostNumbers
 - False
 - Prelude> length lostNumbers
 - 6
 -
 - Prelude> take 3 lostNumbers
 - [4, 8, 15]
 - Prelude> take 0 lostNumbers
 - []
 - Prelude> take 10 lostNumbers
 - [4, 8, 15, 16, 23, 42]
 - Prelude> drop 3 lostNumbers
 - [16, 23, 42]
 - Prelude> drop 10 lostNumbers
 - []
 -
 - Prelude> minimum lostNumbers
 - 4
 - Prelude> maximum lostNumbers
 - 42
 -
 - Prelude> sum [1, 2, 3, 4]
 - 10
 - Prelude> product [1, 2, 3, 4]
 - 24
 -
 - Prelude> elem 8 lostNumbers
 - True
 - Prelude> 10 `elem` lostNumbers
 - False
 -}


{-
 - Intervals:
 -}

{-
 - Prelude> [1..10]
 - [1,2,3,4,5,6,7,8,9,10]
 - Prelude> ['h'..'p']
 - "hijklmnop"
 - Prelude> [2, 4 .. 20]
 - [2,4,6,8,10,12,14,16,18,20]
 - Prelude> [20, 19 .. 1]
 - [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
 - Prelude> [20..1]
 - []
 -
 - Better not to use float numbers in intervals:
 - Prelude> [0.1, 0.3 .. 1]
 - [0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]
 -
 - Infinite intervals:
 - Prelude> take 10 [1, 3 ..]
 - [1,3,5,7,9,11,13,15,17,19]
 - Prelude> take 10 (cycle [1, 2, 3])
 - [1,2,3,1,2,3,1,2,3,1]
 - Prelude> take 4 (repeat 42)
 - [42,42,42,42]
 -
 - Or if you need to repeat something N times:
 - Prelude> replicate 3 42
 - [42,42,42]
 - Prelude> replicate 3 [1, 2]
 - [[1,2],[1,2],[1,2]]
 -}


{-
 - Generators:
 -}

boomBangs xs = [ if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x ]

{-
 - Prelude> [2 * x | x <- [1..10]]
 - [2,4,6,8,10,12,14,16,18,20]
 - Prelude> [x*x | x <- [1..10], x*x >= 50]
 - [64,81,100]
 - Prelude> [x | x <- [1..20], x `mod` 3 == 0]
 - [3,6,9,12,15,18]
 -
 - Prelude> boomBangs [7..14]
 - ["Boom!","Boom!","Bang!","Bang!"]
 -
 - Prelude> [x | x <- [1..20], x /= 15, x /= 17, x /= 19]
 - [1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,18,20]
 -
 - Prelude> [ x + y | x <- [1..3], y <- [10, 100, 1000]]
 - [11,101,1001,12,102,1002,13,103,1003]
 -}
