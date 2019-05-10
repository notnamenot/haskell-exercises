module StudentApp where
 
import System.Exit	--exit

data Student = Student {name::String, age::Int, id::String} deriving (Show, Eq, Read)

type ActionType = [Student] -> IO[Student] 

testList = [Student "Ala" 20 "297123", Student "Ola" 24 "297124"]


main = do
	mainLoop testList	


mainLoop::[Student]->IO()
--mainLoop students =
mainLoop students = do
	printMenu
	choice <- getChoice
	students' <- getAction choice students	
	mainLoop students'
	
	
printMenu::IO()
printMenu = do
	putStrLn "\n1. Dodaj nowego studenta"
	putStrLn "2. Wyświetl wszystkich studentów"
	putStrLn "3. Usuń studenta o zadanym numerze albumu"
	putStrLn "4. Zakończ pracę z programem\n"


getChoice :: IO String
getChoice = getLine 
	
getAction :: String -> ActionType	--dostaje wybór
getAction nr list
	| nr == "1" = addStudent list
	| nr == "2" = showStudentsList list
	| nr == "3" = removeStudent list
	| nr == "4" = finish list

--2
showStudentsList :: ActionType 
showStudentsList list = do
	mapM_ print list  
	return list
--1
addStudent :: ActionType
addStudent list = do
	putStrLn "Podaj imie:"
	name <- getLine
	putStrLn "Podaj wiek:"
	age <- getLine
	let a = (read age :: Int)
	putStrLn "Podaj ID"
	id <- getLine
	return (list ++ [Student name a id])  
--3
removeStudent :: ActionType
removeStudent list = do
	putStrLn "Podaj ID studenta do usunięcia z listy:"
	idi <- getLine	
	return [x | x <- list, StudentApp.id x /= idi ]
--4
finish :: ActionType
finish list = exitSuccess	-- exitWith ExitSuccess




--x <-fmap read getline		--readLn --<$> -fmap
