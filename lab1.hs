head' :: [a] -> a
head' (x:xs) = x

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + (length' xs) 

length'' :: [a] -> Int
length'' list = sum [1 | _ <- list]


take' :: (Ord t1, Num t1) => t1 -> [t] -> [t]
take' n _ | n <= 0  = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs  


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs


append :: [a] -> [a] -> [a]	--(++)
append [] x = x
append (x:xs) y = x:(append xs y)

