import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Data.List 
import Data.List.Split
import Data.Function (on)
import qualified Data.Text as T

--data Tree = Leaf | Node (Tree) (Tree) deriving (Show, Eq)
data Tree = Leaf String | Node Int Float (Tree) (Tree) deriving (Show, Eq)
--data Side = MyLeft | MyRight

level element =  (read (last element) :: Float)  

testFind _ [] = error "Not found testFind, tree probably missing Leafs"
testFind ind (x:xs) = helper ind (x:xs) (0,0) False
                        where helper ind (x:xs) (first, second) firstFound
                                | ind /= (level x) && firstFound == False = helper ind xs (first + 1, second + 1) False
                                | ind == (level x) && firstFound == False = helper ind xs (first, second + 1) True
                                | ind /= (level x) && firstFound == True = helper ind xs (first, second + 1) True
                                | ind == (level x) && firstFound == True = (first, second)

getFstFromFind (x:xs) = fst (testFind ((level x)+1) xs)      
getSndFromFind (x:xs) = snd (testFind ((level x)+1) xs)                            

                                
parseTree (x:xs) 
    | (head x) == "Node" = Node (read (x !! 1) :: Int) (read (x !! 2) :: Float) (parseTree (take (getSndFromFind (x:xs)) xs)) (parseTree (drop (getSndFromFind (x:xs)) xs))
    | (head x) == "Leaf" && (not (null xs)) = error "Not a tree" 
    | (head x) == "Leaf" = Leaf (x !! 1) 
    | otherwise = error "COSKA STRASNE NEDOBRE"


countSpaces (x:xs) = helper (x:xs) 0
                        where helper (x:xs) count 
                                | x == ' ' = helper xs (count + 1)
                                | otherwise = if (count == 0) then 0 else count / 2  

-- GINI
-- prebrané z https://www.youtube.com/watch?v=_L39rN6gz7Y

calculateAverages (x:y:[]) = ((x + y) / 2) : []
calculateAverages (x:y:xs) = ((x + y) / 2) : calculateAverages (y:xs)

-- Gini impurity = 1 - (pravdepodobnost triedy 1)^2 - (pravdepodobnost triedy n)^2
--calculateDispersion inputData threshold classes 
--    | 

-- ((x:xs), (x:ys)) je par zoznamov, pricom fst su ciselne hodnoty a snd su labely 
--          hodnoty na rovnakych indexoch tvoria par
-- listOfTuples je list tuplov, kde fst je label a snd je pocet hodnot, ktore boli
--      rozdelene do tychto labelov podla thresholdu

-- splitByAvg threshold ((x:xs), (y:ys)) listOfTuples =
--     let smaller = listOfTuples
--         bigger = listOfTuples
--     in  if (x <= threshold) 
--             then  splitByAvg (xs, ys) (incrementListOfTuples smaller y)
--             else  splitByAvg (xs, ys) (incrementListOfTuples bigger y)

-- splitByAvg threshold ((x:[]), (y:[])) listOfTuples =   
--     let smaller = listOfTuples
--         bigger = listOfTuples
--     in  if (x <= threshold) 
--             then incrementListOfTuples smaller y
--             else incrementListOfTuples bigger y      

incrementListOfTuples (x:xs) label  
        | (fst x) /= label && xs == [] = error "incrementListOfTuples -- label not found"
        | (fst x) /= label = x : (incrementListOfTuples xs label)
        | (fst x) == label && xs /= [] = (fst x, (snd x) + 1) : xs
        | (fst x) == label && xs == [] = (fst x, (snd x) + 1) : []
        | otherwise = error "incrementListOfTuples -- Critical error"

getSmaller [] _ = error "getSmaller: empty list input"
getSmaller (x:xs) threshold     
    | x <= threshold && xs == [] = x : []
    | x <= threshold = x : (getBigger xs threshold)
    | x > threshold && xs == [] = []
    | x > threshold = (getBigger xs threshold)

getBigger [] _ = error "getBigger: empty list input"
getBigger (x:xs) threshold 
    | x > threshold && xs == [] = x : []
    | x > threshold = x : (getBigger xs threshold)
    | x <= threshold && xs == [] = []
    | x <= threshold = (getBigger xs threshold)    
