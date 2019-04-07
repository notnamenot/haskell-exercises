module BinaryTree
    ( Tree(EmptyTree,Node)
	, treeInsert
	, treeEmpty   
	, isBinary  
	, treeSearch
	, isBalanced
    ) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a, Num a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a  = Node a (treeInsert x left) right
	| x > a  = Node a left (treeInsert x right)



--https://www.haskell.org/tutorial/modules.html
--http://learnyouahaskell.com/making-our-own-types-and-typeclasses

treeEmpty :: Tree a -> Bool
treeEmpty EmptyTree = True
treeEmpty (Node _ _ _) = False


isBinary :: (Ord a) => Tree a -> Bool
isBinary EmptyTree = True
isBinary (Node a EmptyTree EmptyTree) = True
isBinary (Node a EmptyTree t2@(Node c c1 c2))
	| a <= c && isBinary t2 = True
	| otherwise = False
isBinary (Node a t1@(Node b b1 b2)  EmptyTree )
	| a >= b && isBinary t1 = True
	| otherwise = False
isBinary (Node a t1@(Node b b1 b2) t2@(Node c c1 c2)) 
	| a >= b && a <= c && isBinary t1 && isBinary t2 = True 
	| otherwise = False


treeSearch :: (Ord a) => a -> Tree a -> Bool
treeSearch x EmptyTree = False
treeSearch x (Node a _ _)
	| x == a = True 
treeSearch x (Node a t1 t2)
	| treeSearch x t1 || treeSearch x t2 = True
	| otherwise = False


isBalanced :: (Ord a) => Tree a -> Bool --wysokość lewego i prawego poddrzewa każdego węzła różni się co najwyżej o jeden
isBalanced EmptyTree = True
isBalanced t@(Node _ t1 t2) 
	| (countLeft t == countRight t || countLeft t == (countRight t - 1) || countLeft t == (countRight t + 1)) && (isBalanced t1 && isBalanced t2) = True 
	| otherwise = False

--funkcje pomocnicze
countLeft :: Tree a -> Int
countLeft EmptyTree = 0
countLeft (Node _ t1 _) = 1 + (countLeft t1)

countRight :: Tree a -> Int
countRight EmptyTree = 0
countRight (Node _ _ t2) = 1 + (countRight t2)



