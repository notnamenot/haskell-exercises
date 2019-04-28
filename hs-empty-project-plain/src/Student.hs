module Student 
	( Student(Student)
	, fullName
	, listOfStudents
	, nr_student	
	, raport	
	) where

import System.IO  --putStrLn

data Student = Student { firstName::String 
			, lastName::String
			, age::Int 
			} -- deriving (Show,Eq)
			

instance Show Student where
	show (Student fn ln a) = "Student {"++fn++", "++ln++", "++show a++"}"

instance Eq Student where
	Student fn1 ln1 a1 == Student fn2 ln2 a2 = (fn1==fn2) && (ln1==ln2) && (a1==a2) 

listToProcess = [Student "Alicja" "Akla" 21, Student "Batrek" "Bodo" 20, Student "Celina" "Czyzyk" 21, Student "Damian" "Dab"  22, Student "Eustachy" "Elo" 20]

testStudent = Student {firstName="Liz", lastName="Taylor", age=10}
testStudent2 = Student "Ala" "Makota" 12
testStudent3 = testStudent{age=23}
defaultStudent = Student{firstName="None", lastName="None", age=0}
anie = defaultStudent{firstName="Annie"}
--możliwe: 	firstName testStudent

fullName :: Student -> String 
--fullName (Student fn ln _) = fn ++ " " ++ln
fullName student = (firstName student) ++ " " ++ (lastName student)

listOfStudents :: [Student] -> [String]
listOfStudents list = [fullName student | student <- list]
--listOfStudents listToProcess
--map fullName listToProcess

nr_student :: [Student] -> [(Int,Student)]
nr_student list = foldl (\acc x -> acc ++ [(length acc + 1,x)] ) [] list

numberedList = nr_student listToProcess

raport :: (Show a) => [(a, Student)] -> IO ()
raport list = putStrLn $ unlines  [show nr++". student: "++(lastName student)++" "++( take 1 $ firstName student)++" wiek: "++show (age student) | (nr,student) <- list]
--raport numberedList

