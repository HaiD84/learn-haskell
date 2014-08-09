data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

{- Crumb: which way we turned, from what node and other branch -}
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, (LeftCrumb x r):bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, (RightCrumb x l):bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (l, (LeftCrumb x r):bs) = Just (Node x l r, bs)
goUp (r, (RightCrumb x l):bs) = Just (Node x l r, bs)
goUp (_, []) = Nothing

{-
 - *Main> Just (freeTree , []) >>= goRight >>= goLeft >>= goLeft >>= goLeft >>= goRight
 - Nothing
 -}


modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)
{-
 - newTree points at W:
 - let newTree = goLeft $ goRight (freeTree, [])
 - replace it to P:
 - let newTree2 = modify (\_ -> 'P') newTree
 -}

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

{- without monad: -}
{-topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)-}

{- monad: -}
topMost :: Zipper a -> Maybe (Zipper a)
topMost (t, []) = Just (t, [])
topMost z = goUp z >>= topMost



type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)



type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
    Folder "root" [
        File "goat_yelling_like_man.avi" "Baaaa",
        File "pope_time.mkv" "God, save us",
        Folder "pics" [
            File "ape_throwing_up.jpg" "eyu",
            File "watermellon_smash.gif" "smash..",
            File "skull_man(scary).bmp" "Oh"
        ],
        File "dijon_poupon.doc" "best mustard",
        Folder "programs" [
            File "sleepwizard.exe" "10 time to sleep",
            File "owl_bandit.dmg" "mov ax 42h",
            File "not_a_virus" "not a virus, really"
        ],
        Folder "source_code" [
            File "best_hs_prog.hs" "main = print (fix error)",
            File "random.hs" "main = print 4"
        ]
    ]

{- one crumb: folder name and files before and after focused element -}
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, (FSCrumb folderName ls rs):bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

{- just for ease reading: -}
x -: f = f x

{-
 - *Main> let newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
 - *Main> fst newFocus
 - File "skull_man(scary).bmp" "Oh"
 - *Main> let newFocus2 = newFocus -: fsUp -: fsTo "watermellon_smash.gif"
 - *Main> fst newFocus2
 - File "watermellon_smash.gif" "smash.."
 -}

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)
{-
 - *Main> let newFocus = (myDisk, []) -: fsTo "pics" -: fsRename "scip" -: fsUp
 -}

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = (Folder folderName (item:items), bs)
{-
 - *Main> let newFocus = (myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp
 -}
