--KOL 17/18
import Data.List	--sortBy
import qualified Data.Set as S
--1
data Pizza = Pizza {pizzaId :: Int, pizzaName :: String, price :: Double, size :: Int}  deriving (Show)	--,Eq

data Topping = Topping {ofPizzaId :: Int, topName :: String, veggie :: Bool, alergens :: [String]} -- deriving (Show,Eq)

data PizzaWithToppings = PizzaWithToppings {pizza :: Pizza, listOfToppings :: [Topping]} deriving(Show)

picka1 = Pizza {pizzaId = 1, pizzaName = "Capri", price = 10.0, size = 30}
picka2 = Pizza 2 "Veg" 15.0 30

top1 = Topping 1 "cheese" False ["uno", "due"]

--2
instance Show Topping where
	show (Topping id name veggie alergens) = "Topping{" ++ show id ++ ", " ++ name ++ ", " ++ show veggie ++ ", " ++ show alergens ++ "}" --unwords alergens

--3
instance Eq Pizza where
	Pizza id1 name1 price1 size1 == Pizza id2 name2 price2 size2 = id1==id2 && name1==name2 && price1==price2 && size1==size2

--4
pizzas :: [Pizza]
pizzas = [Pizza 26453 "Pleistocenska" 32.8 42, Pizza 33596 "Kebab" 29.9 42,Pizza 33596 "Kebab" 32.9 45]

toppings :: [Topping]
toppings = [Topping 33596 "Bekon" False [], Topping 33596 "Ser Mozarella" False ["laktoza","mleko"],Topping 26453 "Papryka" True ["cos"], Topping 26453 "Zielone Soczyste Brokuły" True [],Topping 26453 "Pomidor" True []]

-- 4.1
type Picka = [Pizza] -> [Pizza]

sortPizzasByPrice :: Picka
sortPizzasByPrice pizzas = sortBy (\(Pizza _ _ price1 _) (Pizza _ _ price2 _) -> compare price1 price2) pizzas --compare `on` Pizza{price}

-- 4.2 
sortPizzasByNameAndSize :: Picka
sortPizzasByNameAndSize pizzas = sortBy (\(Pizza _ _ _ size1) (Pizza _ _ _ size2) -> compare size1 size2) . reverse $ sortBy (\(Pizza _ name1 _ _) (Pizza _ name2 _ _) -> compare name1 name2) pizzas

-- 4.3 
sortToppings :: [Topping] -> [Topping]
sortToppings toppings = sortBy sortByNameAsc (sortBy sortToppingsByVeggie (sortBy sortNumberOfAlergens toppings))

--sortToppings :: [Topping] -> [Topping]
--sortToppings = sortBy (\(Topping _ _ _ a1) (Topping _ _ _ a2) -> compare (length a1) (length a2)) . sortBy (\(Topping _ _ v1 _) (Topping _ _ v2 _) -> compare v2 v1) . sortBy (\(Topping _ n1 _ _ ) (Topping _ n2 _ _) -> compare n1 n2)


sortNumberOfAlergens :: Topping -> Topping -> Ordering
sortNumberOfAlergens (Topping _ _ _ aler1) (Topping _ _ _ aler2)
	| length aler1 == length aler2 = EQ
	| length aler1 < length aler2 = LT
	| length aler1 > length aler2 = GT

sortToppingsByVeggie :: Topping -> Topping -> Ordering
sortToppingsByVeggie (Topping _ _ veg1 aler1) (Topping _ _ veg2 aler2)
	| length aler1 == length aler2 && veg1 == veg2 = EQ
	| length aler1 == length aler2 && veg1 == True = LT
	| length aler1 == length aler2 && veg1 == False = GT
	| length aler1 /= length aler2 = EQ 

sortByNameAsc :: Topping -> Topping -> Ordering
sortByNameAsc (Topping _ name1 veg1 aler1) (Topping _ name2 veg2 aler2) 
	| name1 == name2 = EQ
	| length aler1 == length aler2 && veg1 == veg2 && name1 < name2  = LT
	| length aler1 == length aler2 && veg1 == veg2 && name1 > name2 = GT
	| otherwise = EQ 


--5

toPizzaWithToppings :: Pizza -> Topping -> Maybe PizzaWithToppings
toPizzaWithToppings picka@(Pizza idP nameP price size) top@(Topping idT nameT veg aler)
	| idP == idT = Just (PizzaWithToppings picka [top])
	| otherwise  = Nothing 

--6

findById :: [Topping] -> Int -> [Topping] 
findById toppings id = filter (\(Topping idt _ _ _) -> idt == id) toppings 

-- 7

mapBy :: (Pizza -> a) -> [a]
mapBy f = map f pizzas

--8 
--suml xs = foldl (\acc x -> acc + x) 0 xs -- (+)
--sumr xs = foldr (\x acc -> acc + x) 0 xs
reduceBy1 :: ([(String,Int)] -> Char -> [(String,Int)]) -> [(String,Int)] 
reduceBy1 f = foldl f [("napis",0)] "dlugi napis..."   


-- 9 

--reduceBy2 :: Num a => (Set a1 -> a -> Set a1) -> Set a1
--reduceBy2 f = foldl S.empty [5, 6, 1, 2] 

-- 10

join :: [Pizza] -> [Topping] -> [PizzaWithToppings]
join pizzas toppings = map (\picka@(Pizza id name price size) -> (PizzaWithToppings picka $ findById toppings id )) pizzas

-- 11

class (Ord a) => Range a where
	fromInclusive :: (a,a) -> a
	toExclusive :: (a,a) -> a
	isInRange :: a -> (a,a) -> Bool
	isInRange x (a,b) 	-- = x >= a && x <= b
		| x >= a && x <= b = True
		| otherwise = False
--instance Ord (Range Int) where
--	Range (a1,b1) `compare` Range (a2,b2) = a1 `compare` a2 

instance Range Int where 
	fromInclusive (first,_) = first
	toExclusive (_,second) = second

-- 12
data PriceRange a = PriceRange (a,a) deriving (Show)
instance Ord PriceRange Int where
	PriceRange (a1,b1) `compare` PriceRange (a2,b2) = a1 `compare` a2 


