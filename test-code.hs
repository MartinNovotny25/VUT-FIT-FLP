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

--calculateAverages (x:y:[]) = ((x + y) / 2) : []
--calculateAverages (x:y:xs) = ((x + y) / 2) : calculateAverages (y:xs)



makePair value label = (value, label)


-- Spocita, kolko hodnot labelov bolo mensich alebo vacsich ako treshold
-- hodnoty uz su spocitane
-- vstupom je uz vytvorene pole hodnot, ktore boli bud mensie alebo vacsie 
-- a je ich treba iba spocitat


--       value listOfTuples
countLabels x (y:[])
    | (snd x) == (snd y) = ((fst y) +1, snd y) : []
    | otherwise =  []
countLabels x (y:ys) 
    | (snd x) == (snd y) = ((fst y) +1, snd y) : countLabels x ys
    | otherwise =  countLabels x ys



-- funkcia funguje tak, ze sa zo zoznamu roztriedenych prvkov vytahuju dvojice po jednom
testFold (x:[]) list = (countLabels x list) : []
testFold (x:xs) list = (countLabels x list) : testFold xs list     

incrementListOfTuples (x:xs) label  
        | (snd x) /= label && xs == [] = error "incrementListOfTuples -- label not found"
        | (snd x) /= label = x : (incrementListOfTuples xs label)
        | (snd x) == label && xs /= [] = ((fst x)+1, snd x) : xs
        | (snd x) == label && xs == [] = ((fst x)+1, snd x) : []
        | otherwise = error "incrementListOfTuples -- Critical error"

-- TODO - ASI TO UPLNE ZMAZ A POUZI PROSTE FILTER
-- Todo - Mozno bude stacit iba jedna funckia a potom s vysledkom pouzit filter na povodny zoznam
 
getSmaller [] _ = error "getSmaller: empty list input"
getSmaller (x:xs) threshold     
    | (fst x) <= threshold && xs == [] = x : []
    | (fst x) <= threshold = x : (getSmaller xs threshold)
    | (fst x) > threshold && xs == [] = []
    | (fst x) > threshold = (getSmaller xs threshold)

getBigger [] _ = error "getBigger: empty list input"
getBigger (x:xs) threshold 
    | (fst x) > threshold && xs == [] = x : []
    | (fst x) > threshold = x : (getBigger xs threshold)
    | (fst x) <= threshold && xs == [] = []
    | (fst x) <= threshold = (getBigger xs threshold)    



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
    where 
        helper [] _ = [] 
        helper (x:xs) seen
            | x `elem` seen = helper xs seen  
            | otherwise = (0, x) : helper xs (x : seen)  


