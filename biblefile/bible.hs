import System.IO  
import Data.List	--sortBy
import Data.Function	--on
import qualified Data.Map as Map  

main = do  
	--handle <- openFile "bib.txt" ReadMode  
	--contents <- hGetContents handle  
	contents <- readFile "bib.txt" --"bible+shakes.nopunc"
	putStrLn "\n10 najczęściej występujących wyrazów:"  
	let mostPopular = mostPop contents
	print mostPopular
	putStrLn "\nLiczba lini bez tych  wyrazów: " 
	print (uniLines contents mostPopular)
	--hClose handle  

countWords :: [String] -> Map.Map String Int -> Map.Map String Int
countWords [] mapa = mapa
countWords (w:wrds) oldmap = countWords wrds (Map.insertWith (+) w 1 oldmap) 

mostPop :: String -> [(String,Int)]
mostPop input = 
    let wrds = words input
        countedWords = Map.toList (countWords wrds Map.empty)
        sorted10 = take 10 . reverse $ sortBy (compare `on` snd) countedWords -- \ x y -> compare (snd x) (snd y)
    in sorted10

uniLines :: String -> [(String,Int)] -> Int
uniLines input popular =
    let lns = lines input
        popularWords = [wrds | wrds <- map fst popular]
        num = sum [1 | line <- lns, popularWords `intersect` (words line) == []]
    in num


{--
isWordInLine :: String -> String -> Bool
isWordInLine word line = 
    | word `elem` (words line) = True
    | otherwise = False
--}


{--myCompare :: (Ord v) => Map.Map k v -> Map.Map k v -> Ordering
myCompare (Map.singleton k1 v1) (Map.singleton k2 v2)
	| v1 > v2 = GT
	| v1 == v2 = EQ	
	| v1 < v2 = LT
--}
-- https://stackoverflow.com/questions/2349798/in-haskell-how-can-i-use-the-built-in-sortby-function-to-sort-a-list-of-pairst
--https://stackoverflow.com/questions/22412258/get-the-first-element-of-each-tuple-in-a-list-in-python/22412308
-- https://stackoverflow.com/questions/51179828/updating-items-in-a-haskell-map-how
-- https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/containers-0.3.0.0/Data-Map.html




