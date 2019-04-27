import System.IO  
import Data.List	--sortBy
--import Data.List.Extra
import Data.Function	--on
--import qualified Data.Text as T 
import qualified Data.Map as Map  

main = do  
	--handle <- openFile "bib.txt" ReadMode  
	--contents <- hGetContents handle  
	contents <- readFile "coli.txt"	--"ecoli.fa"
	putStrLn "10 najpopularniejszych 9-merów"
	let mostPopular = mostPop contents
	print mostPopular
	--print (substring 9 9 contents)

--mostPop :: String -> [(String,Int)]
mostPop :: String -> [(String, Int)] 
mostPop input = 
    let list = [substring 9 (i+9) (filter (/= '\n') input) | i <- [0..((length input) - 9 )]]
        countedWords = Map.toList (countWords list Map.empty)	
        sorted10 = take 10 . reverse $ sortBy (compare `on` snd) countedWords -- \ x y -> compare (snd x) (snd y)
    in sorted10

--substring :: Int -> Int -> String -> String
substring a b text = takeEnd a (take b text)

takeEnd :: Int -> [a] -> [a]
takeEnd i xs = f xs (drop i xs)
    where f (x:xs) (y:ys) = f xs ys
          f xs _ = xs


countWords :: [String] -> Map.Map String Int -> Map.Map String Int
countWords [] mapa = mapa
countWords (w:wrds) oldmap = countWords wrds (Map.insertWith (+) w 1 oldmap) 

--https://stackoverflow.com/questions/9475241/split-string-every-nth-character
-- https://stackoverflow.com/questions/3651144/comparing-lists-in-haskell-or-more-specifically-what-is-lexicographical-order
-- https://stackoverflow.com/questions/53706714/grouping-a-list-of-lists-by-their-first-element

--https://stackoverflow.com/questions/572549/difference-between-git-add-a-and-git-add
