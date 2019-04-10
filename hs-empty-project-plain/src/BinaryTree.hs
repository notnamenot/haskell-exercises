module BinaryTree
    ( Tree(EmptyTree,Node)
	, treeInsert
	, treeEmpty   
	, isBinary  
	, treeSearch
	, isBalanced
	, traverseLVR'
	, traverseVLR 
	, traverseLVR
	, traverseLRV
	, treeToString
	, leaves
	, nnodes
	, nsum
	, tmap
	, treeRemove
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
treeSearch x (Node a t1 t2)
	| x == a = True
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
countRight (Node _ _ t2) = 1 + (countRight t2)  -- ??mogą iść na zmianę



traverseLVR :: Tree a -> [a] 
traverseLVR EmptyTree = []
traverseLVR (Node a t1 t2) = (traverseLVR t1) ++ [a] ++ (traverseLVR t2)

traverseVLR :: Tree a -> [a] 
traverseVLR EmptyTree = []
traverseVLR (Node a t1 t2) = [a] ++ (traverseLVR t1) ++ (traverseLVR t2)

traverseLRV :: Tree a -> [a] 
traverseLRV EmptyTree = []
traverseLRV (Node a t1 t2) = (traverseLVR t1) ++ (traverseLVR t2) ++ [a]

traverseVRL :: Tree a -> [a] 
traverseVRL EmptyTree = []
traverseVRL (Node a t1 t2) = [a] ++ (traverseLVR t2) ++ (traverseLVR t1)

traverseRVL :: Tree a -> [a] 
traverseRVL EmptyTree = []
traverseRVL (Node a t1 t2) = (traverseLVR t2) ++ [a] ++ (traverseLVR t1)

traverseRLV :: Tree a -> [a] 
traverseRLV EmptyTree = []
traverseRLV (Node a t1 t2) = (traverseLVR t2) ++ (traverseLVR t1) ++ [a]


traverseLVR' :: Tree a -> [Tree a] 
traverseLVR' EmptyTree = []
traverseLVR' n@(Node a t1 t2) = (traverseLVR' t1) ++ [n] ++ (traverseLVR' t2)



treeToString :: (Show a) => Tree a -> String
treeToString EmptyTree = ""
treeToString (Node a EmptyTree EmptyTree) = show a
treeToString (Node a t1 t2) = (show a) ++ "(" ++ (treeToString t1)  ++ "," ++ (treeToString t2) ++ ")"



leaves :: (Eq t) => Tree t -> [Tree t]
leaves t = [ l | l <- (traverseLVR' t), isLeaf l ]
isLeaf (Node a t1 t2)
	| t1 == EmptyTree && t2 == EmptyTree = True
	| otherwise = False



nnodes :: Tree a -> Int		--liczba węzłów
nnodes EmptyTree = 0
nnodes (Node _ t1 t2) = 1 + (nnodes t1) + (nnodes t2)



nsum :: (Num t) => Tree t -> t	--suma w węzłach
nsum EmptyTree = 0
nsum t = foldr (+) 0 (traverseLVR t) 



--map :: (a -> b) -> [a] -> [b]
tmap :: (a -> b) -> Tree a -> Tree b
tmap f EmptyTree = EmptyTree
tmap f (Node a t1 t2) = Node (f a) (tmap f t1) (tmap f t2)


treeRemove :: (Eq a, Ord a) => a -> Tree a -> Tree a
treeRemove _ EmptyTree = EmptyTree
treeRemove x t@(Node a t1 t2)
	| treeSearch x t == False = t		--jeśli nie ma takiego el
	| x == a && isLeaf t = EmptyTree	--liść
	| x == a && t1 == EmptyTree = t2	--jeśli ma tylko prawe dziecko	
	| x == a && t2 == EmptyTree = t1 	--jeśli ma tylko prawe dziecko	
	| x == a = Node (findMin t2) t1 (treeRemove (findMin t2) t2) --minimum prawgo dzicka wchodzi na miejsce usuwanego elementu
	| otherwise = Node a (treeRemove x t1) (treeRemove x t2)

--funkcje pomocnicze
findMin :: Tree a -> a
findMin (Node a EmptyTree EmptyTree) = a
findMin (Node a t1 _) = findMin t1

findMax :: Tree a -> a
findMax (Node a EmptyTree EmptyTree) = a
findMax (Node a _ t2) = findMax t2



--merge :: (Ord a, Num a) => Tree a -> Tree a -> Tree a
merge EmptyTree t2 = t2
merge t1 EmptyTree = t1
merge t1 t2 = merge (treeInsert (head (traverseLVR t2)) t1) (treeRemove (head (traverseLVR t2)) t2)









