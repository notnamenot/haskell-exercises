suml :: (Num a) => [a] -> a
suml xs = foldl (\acc x -> acc + x) 0 xs -- (+)

sumr :: (Num a) => [a] -> a
sumr xs = foldr (\x acc -> acc + x) 0 xs 
--sumr = foldr (+) 0

productl :: (Num a) => [a] -> a		--(Foldable t, Num a) => t a -> a
productl xs = foldl (*) 1 xs		--(\acc x -> acc * x)

productr :: (Num a) => [a] -> a		
productr = foldr (\x acc -> acc + x) 1 
--product' = foldr1 (*)

reversel :: [a] -> [a]
reversel xs = foldl (\acc x -> [x] ++ acc) [] xs
--reverse' = foldl (\acc x -> x : acc) []  

reverser :: [a] -> [a]
reverser xs = foldr (\x acc -> acc ++ [x]) [] xs

andl :: [Bool] -> Bool
andl xs = foldl (\acc x -> if acc == True && x == True then acc else False) True xs

andr :: [Bool] -> Bool
andr xs = foldr (\x acc -> if acc == True && x == True then acc else False) True xs

orl :: [Bool] -> Bool
orl xs = foldl (\acc x -> if acc == True then True else x) False xs

orr :: [Bool] -> Bool
orr xs = foldr (\x acc -> if acc == True then True else x) False xs

headl :: [a] -> a
headl (s:xs) = foldl (\acc _ -> acc) s xs 

headr :: [a] -> a
headr l@(s:xs) = foldr (\x _ -> x) s l
--headr = foldr1 (\x _ -> x) 

lastl :: [a] -> a
lastl (s:xs) = foldl (\_ x -> x) s xs
--lastl = foldl1 (\_ x -> x)  



{-
mynextlast [] = error "no empty lists allowed"
mynextlast (x:[])  = x
mynextlast (x:xs) = mynextlast xs

mylast :: [a] -> Maybe a
mylast [] = Nothing
mylast (x:[]) = Just x
mylast (x:xs) = mylast xs
-}


map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs  
--map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  

filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []



{-
-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest
foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 

-- if the list is empty, the result is the initial value; else
-- we recurse immediately, making the new initial value the result
-- of combining the old initial value with the first element.
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs
-}

