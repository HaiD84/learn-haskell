import qualified Data.Map as Map


data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int
} deriving (Show, Eq, Read)

{-
 - *Main> :t Person
 - Person :: String -> String -> Int -> Person
 - *Main> :t firstName
 - firstName :: Person -> String
 -
 - *Main> let guy = Person { firstName="Foo", lastName="Bar", age=1 }
 - *Main> guy
 - Person {firstName = "Foo", lastName = "Bar", age = 1 }
 - *Main> lastName guy
 - "Bar"
 -}


data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x y z) `vplus` (Vector k l m) = Vector (x + k) (y + l) (z + m)

scalarProd :: (Num a) => Vector a -> Vector a -> a
(Vector x y z) `scalarProd` (Vector k l m) = x * k + y * l + z * m

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector x y z) `vmult` m = Vector (x * m) (y * m) (z * m)



mikeD = Person { firstName = "Michael", lastName = "Diamond", age = 43 }
{-
 - *Main> mikeD == mikeD
 - True
 - *Main> Person { firstName = "Michael", lastName = "Diamond", age = 43 } == mikeD
 - True
 -}
mysteryDude = "Person { firstName = \"Bruce\"" ++
    ", lastName = \"Wayne\"" ++
    ", age = 40 }"
{-
 - Tell Haskell what type to read:
 - *Main> read mysteryDude :: Person
 - Person {firstName = "Bruce", lastName = "Wayne", age = 40}
 -
 - Or Haskell can guess it by further usage:
 - *Main> read mysteryDude  == mikeD
 - False
 -}


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

{-
 - Eq and Ord classes:
 - *Main> Monday == Monday
 - True
 - *Main> Saturday > Friday
 - True
 -
 - Bounded class:
 - *Main> minBound :: Day
 - Monday
 - *Main> maxBound :: Day
 - Sunday
 -
 - Enum class:
 - *Main> succ Tuesday
 - Wednesday
 - *Main> pred Saturday
 - Friday
 - *Main> [Monday .. Friday]
 - [Monday,Tuesday,Wednesday,Thursday,Friday]
 - *Main> [minBound .. maxBound] :: [Day]
 - [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
 -}


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker #" ++ show lockerNumber ++ " is not exists"
    Just (state, code) ->
        if state /= Taken
            then Right code
            else Left $ "Locker #" ++ show lockerNumber ++ " is occupied"

lockers :: LockerMap
lockers = Map.fromList
    [ (100, (Taken, "ZE06I"))
    , (101, (Free, "BY57R"))
    , (103, (Taken, "NA81D"))
    ]
{-
 - *Main> lockerLookup 101 lockers
 - Right "BY57R"
 - *Main> lockerLookup 103 lockers
 - Left "Locker #103 is occupied"
 - *Main> lockerLookup 104 lockers
 - Left "Locker #104 is not exists"
 -}
