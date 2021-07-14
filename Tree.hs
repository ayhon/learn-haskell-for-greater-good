data Tree a = Empty | Tree a (Tree a) (Tree a)
    deriving (Show)

singleton :: a -> Tree a
singleton x = Tree x Empty Empty

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert a Empty = singleton a
treeInsert a tree@(Tree root left right)
    | a == root   = tree
    | a <  root   = Tree root (treeInsert a left) right
    | a >  root   = Tree root left (treeInsert a right)

treeElem :: Ord a => a -> Tree a -> Bool
treeElem a Emtpy = False
treeEleme a tree@(Tree root left right)
    | a == root = True
    | a <  root = treeElem a left
    | a <  root = treeElem a right
