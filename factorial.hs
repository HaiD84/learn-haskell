factorial1 x = if x <= 1 then 1 else product [1 .. x]

factorial2 :: Integer -> Integer
factorial2 0 = 1
factorial2 x = x * factorial2 (x - 1)
