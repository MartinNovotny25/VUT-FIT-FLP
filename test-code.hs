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

-- 1. krok - musime vypocitat weighted gini index pre kazdy stlpec
    -- 1. vypocitat priemery susednych prvkov - fcia calculateAverages vypocita priemery
    --                                        - a vrati ich v poli 

    -- AKTUALNE
    -- 2. Pre kazdy jeden priemer urcit, kolko poloziek pojde dolava alebo doprava podla
    -- hodnoty daneho stlpca v polozke


    -- 3. Potom vypocitat Gini purity value podla toho, kolko labelov islo doprava a kolko dolava
    -- Tzn. ak doprava isli 2 A, 3 B a 1 C - GINI pre leaf bude 
    --                            1 - (2/6)^2 - (3/6)^2 - (1/6)^2
    -- obdobne pre druhu vetvu
    -- potom sa spocita weighted gini a podla toho sa vyberie podla ktoreho stlpca sa bude rozhodovat

-- GINI
-- prebrané z https://www.youtube.com/watch?v=_L39rN6gz7Y


incrementListOfTuples (x:xs) label  
        | (snd x) /= label && xs == [] = error "incrementListOfTuples -- label not found"
        | (snd x) /= label = x : (incrementListOfTuples xs label)
        | (snd x) == label && xs /= [] = ((fst x)+1, snd x) : xs
        | (snd x) == label && xs == [] = ((fst x)+1, snd x) : []
        | otherwise = error "incrementListOfTuples -- Critical error"



-- Iny postup - vypocitat priemer po jednom a tak pocitat gini index
calcAverage x y = (x+y)/2
--splitByAvg list xs = filter () 

-- PRIKLAD VSTUPU
-- callIncrement (extractLabels [["7","N"],["12","N"],["18","Y"],["35","Y"],["38","Y"],["50","N"],["83","N"]]) (map snd [(1, "N"), (2, "Y"), (3,"N")])
-- extractLabels vytiahne unikatne páry (0, label), pricom fst sa bude inkrementovat vzdy, pokila bude rovnaky label v druhom zozname
-- druhy zoznam su hodnoty, ktore boli splitnute podla tresholdu
callIncrement toIncrement (y:[]) = incrementListOfTuples toIncrement y
callIncrement toIncrement (y:ys) = callIncrement (incrementListOfTuples toIncrement y) ys

extractLabels :: [[String]] -> [(Int, String)]
extractLabels lists = helper (map last lists) []
    where   helper [] _ = [] 
            helper (x:xs) seen
                | x `elem` seen = helper xs seen  
                | otherwise = (0, x) : helper xs (x : seen)  

calculateGini totalNumber list = 1 - (helper list totalNumber)
    where helper (x:xs) totalNumber
            | xs == [] = (((fromIntegral (fst x))/ (fromIntegral (totalNumber)))^2)
            | xs /= [] = (((fromIntegral (fst x)) / (fromIntegral (totalNumber)))^2) + (helper xs totalNumber)     
            | otherwise = error "calculateGini - CRITICAL ERROR"        

totalNumber (x:xs) 
    | xs == [] = (fst x)
    | xs /= [] = (fst x) + totalNumber xs
    | otherwise = error "totalNumber - CRITICAL ERROR"

-- listSizes ginies
calculateWeightedGini (x:xs) (y:ys) allOccurences
    | (not $ null xs) && (not $ null ys) = ((x / allOccurences)*y) + calculateWeightedGini xs ys allOccurences
    | (null xs) && (null ys) = ((x / allOccurences)*y)
    | (not $ null xs) && (null ys) = error "calculateWeightedgGini - ERROR, GINIES EMPTY"
    | (null xs) && (not $ null ys) = error "calculateWeightedgGini - ERROR, LIST_SIZES EMPTY"
    | otherwise = error "calculateWeightedgGini - CRITICAL ERROR"


