data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node x l r) = Node (f x) (treeMap f l) (treeMap f r)

treeTraverseD :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseD _ acc Empty = acc
treeTraverseD f acc (Node x l r) =
    let accL = treeTraverseD f acc l
        accX = f x accL
    in treeTraverseD f accX r

treeTraverseW :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseW _ acc Empty = acc
treeTraverseW f acc tree = go [tree] acc
      where
    go [] accNew = accNew
    go (Empty:xs) accNew = go xs accNew
    go (Node x l r : xs) accNew =
        let accNewNew = f x accNew
        in go (xs ++ [l, r]) accNewNew

t :: Tree String
t = Node "Eins"
        (Node "Zwei"
            (Node "Vier" Empty Empty)
            (Node "Fuenf" Empty Empty))
        (Node "Drei"
            (Node "Sechs" Empty Empty)
            (Node "Sieben" Empty Empty))
            
-- prettyPrint :: Show a => Tree a -> String
-- prettyPrint Empty = ""
-- prettyPrint (Node x l r) = go "" "" (Node x l r)

prettyDFS :: Show a => Tree a -> String
prettyDFS tree = go 0 tree
  where
    go _ Empty = ""
    go depth (Node x l r) =
        replicate (depth * 4) ' ' ++ show x ++ "\n"
        ++ go (depth + 1) l
        ++ go (depth + 1) r

prettyPrint :: Show a => Tree a -> String
prettyPrint Empty = ""
prettyPrint (Node x l r) = show x ++ "\n" ++ go "" [(l, True), (r, False)]
  where
    go _ [] = ""
    go prefix ((Empty, _):xs) = go prefix xs
    go prefix ((Node y l r, isLeft):xs) =
        prefix ++ (if isLeft then "├── " else "└── ") ++ show y ++ "\n"
        ++ go (prefix ++ (if isLeft then "│   " else "    "))
              [(l, True), (r, False)]
        ++ go prefix xs

main :: IO ()
main = do
    print t
    putStrLn(prettyDFS t)
    putStrLn(prettyPrint t)
    print(treeMap (++ "!") t)
    print(treeSize t)
    print(treeTraverseD (++) "" t)
    print(treeTraverseW (++) "" t)
    