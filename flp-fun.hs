-- FLP Proj 1 - Klasifikačné stromy
-- Autor: Martin Novotný Mlinárcsik
-- xlogin: xnovot1r
-- 29/02/2024
-- VUT FIT

import System.Environment (getArgs)
import System.IO (readFile)
import Data.List 
import Data.List.Split

-- Datový typ strom
-- Leaf v sebe obsahuje triedu
-- Node v sebe obsahuje ID, treshold hodnotu a dvoch potomkov
-- Struktura prebrana z Learn You a Haskell
-- https://learnyouahaskell.github.io/making-our-own-types-and-typeclasses.html#recursive-data-structures

data Tree = Leaf String | Node Int Float (Tree) (Tree) deriving (Show, Eq)

-- Main ----------------------------------------------------------------------

main = do 

    args <- getArgs
    case args of [] -> error "Empty"
                 "-1" : treeInput : [dataInput] -> do 
                                                    treeStr <- readFile treeInput   
                                                    dataStr <- readFile dataInput
                                                    doClass treeStr dataStr
                 "-2" : [dataInput] -> do
                                         dataStr <- readFile dataInput
                                         print $ (map (splitOn (",")) $ lines dataStr) 
                                         doTrain (map (splitOn (",")) $ lines dataStr)                                  
                 _ -> error "Argumenty coska nedobre"


------ Vypocet GINI index ---------------------------------------
-- prebrané z https://www.youtube.com/watch?v=_L39rN6gz7Y

--doTrain dataInput = calculateAverages (getFloat dataInput)
-- Tato cast vrati prvy list listov stringov ako list floatov
doTrain dataInput = --print $ glueTogether dataInput
    let maxIndex = (length (dataInput !! 0))
        --in print $ (calcAllWeights getPairs)
        --in print $ extractLabels getPairs
        in print $ getPairs 0 maxIndex dataInput


getPairs currentIndex maxIndex dataInput  
    | currentIndex == (maxIndex-1) = []
    | currentIndex /= (maxIndex-1) = (zipWith makePair ((parseColumns dataInput currentIndex maxIndex)) ((parseColumns dataInput (maxIndex-1) maxIndex))) : (getPairs (currentIndex + 1) maxIndex dataInput)
                        where makePair x y = (x,y)

parseColumns input index maxIndex  
    | index /= (maxIndex - 1) = map (!! index) input : parseColumns input (index+1) maxIndex
    | otherwise = (map (!! index) input) : []  

readAsFloat = (read :: String -> Float) 

incrementListOfTuples (x:xs) label  
        | (snd x) /= label && xs == [] = error "incrementListOfTuples -- label not found"
        | (snd x) /= label = x : (incrementListOfTuples xs label)
        | (snd x) == label && xs /= [] = ((fst x)+1, snd x) : xs
        | (snd x) == label && xs == [] = ((fst x)+1, snd x) : []
        | otherwise = error "incrementListOfTuples -- Critical error"
    
calcAllWeights inputList = iterateOver inputList ((length inputList) -1)
                    where iterateOver inputList iterations
                                | iterations /= 1 = (glueTogether inputList iterations) : (iterateOver inputList (iterations-1) )
                                | iterations == 1 = (glueTogether inputList iterations) : []


glueTogether inputList index = 
    let average = (calcAverage (read (fst (inputList !! index)) :: Float) (read (fst (inputList !! (index -1)))) :: Float)
        splitList = splitByAvg average inputList
        occurencesSmaller = callIncrement (extractLabels inputList) (map snd (fst splitList))
        occurencesBigger =  callIncrement (extractLabels inputList) (map snd (snd splitList))
        totalSmaller = totalNumber occurencesSmaller
        totalBigger = totalNumber occurencesBigger
        giniSmaller = calculateGini totalSmaller occurencesSmaller
        giniBigger = calculateGini totalBigger occurencesBigger
        allOccurences = totalBigger + totalSmaller
        weightedGini = calculateWeightedGini [totalSmaller, totalBigger] [giniSmaller, giniBigger] allOccurences
        in (weightedGini) :: Float

-- Iny postup - vypocitat priemer po jednom a tak pocitat gini index
calcAverage x y = (x+y)/2
splitByAvg threshold list = ([x | x <- list, (read (fst x) :: Float) <= threshold], [x | x <- list, (read (fst x) :: Float) > threshold])

-- PRIKLAD VSTUPU
-- callIncrement (extractLabels [["7","N"],["12","N"],["18","Y"],["35","Y"],["38","Y"],["50","N"],["83","N"]]) (map snd [(1, "N"), (2, "Y"), (3,"N")])
-- extractLabels vytiahne unikatne páry (0, label), pricom fst sa bude inkrementovat vzdy, pokila bude rovnaky label v druhom zozname
-- druhy zoznam su hodnoty, ktore boli splitnute podla tresholdu
callIncrement toIncrement (y:[]) = incrementListOfTuples toIncrement y
callIncrement toIncrement (y:ys) = callIncrement (incrementListOfTuples toIncrement y) ys

extractLabels lists = helper (map snd lists) []
    where   helper [] _ = [] 
            helper (x:xs) seen
                | x `elem` seen = helper xs seen  
                | otherwise = (0, x) : helper xs (x : seen)  

calculateGini totalNumber list = 1 - (helper list totalNumber)
    where helper (x:xs) totalNumber
            | xs == [] = ((((fst x))/ ( (totalNumber)))^2)
            | xs /= [] = ((( (fst x)) / ((totalNumber)))^2) + (helper xs totalNumber)     
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






------  Klasifikácia ------------------------------------------

-- Spracuje vstupy a vykoná klasifikáciu
doClass treeInput dataInput = putStrLn $ classifyAll (parseClassificationValues dataInput) $ parseTree $ map (removeFluff) $ splitInputByWords treeInput

-- classifyAll je pomocná funckia, ktorá aplikuje funckiu classification pre každý
-- zoznam v zozname hodnôť pre klasifikáciu
classifyAll (x:xs) tree
    | xs == [] = classification x tree
    | otherwise = (classification x tree) ++ "\n" ++ classifyAll (xs) tree 

-- classification porovnáva treshold hodnotu aktuálneho uzlu so vstupom
-- Mensia rovná -> ľavý podstrom, väčšia -> pravý podstrom
-- Ak narazíme na Leaf, vrátime triedu
classification [] (Node a b left right) = error "Ended on a node"
classification (x:xs) (Node a b left right) 
    | x <= b = classification xs left
    | otherwise = classification xs right
classification x (Leaf a) = a    



------  Parse text  ------------------------------------------------------------------
-- splitInputByWords dostane prečítaný vstupný reťazec, s pomocou addNestLevel appendne každému
-- prvku stromu jeho hĺbku zanorenia. Keďže addNestLevel vráti zoznam retazcov, pouzijeme map words
-- a kazdy reťazec v zozname rozdelíme na slová
splitInputByWords inputString = (map words $ addNestLevel inputString)

-- addNestLevel pridáva na koniec každého prvku stromu hĺbku  zanorenia.
-- countSpaces vráti zoznam s odpovedajúcimi hĺbkami zanorenia každého prvku.
-- Pomocou zipWith prikonkatenujeme hĺbky zanorenia každému prvku stromu.
-- Prikonkatenujeme vo formáte:

--                      originalString ++ ", " ++ show y
-- keďže v povodnom reťazci sú jednotlivé položky prvku oddelené čiarkou. Show y konvertuje
-- Int hodnotu na String. Čiarka je odstránea v následujúcich krokoch.
                            
addNestLevel list =  zipWith (\x y -> x ++ ", " ++ show y) (lines list) (map countSpaces (lines list))
    where countSpaces (x:xs) = helper (x:xs) 0
                        where helper (x:xs) count 
                                | x == ' ' = helper xs (count + 1)
                                | otherwise = if (count == 0) then 0 else count / 2  

-- removeFluff odstráni z jednotlivých retazcov znaky ':' a ','.
removeFluff contents = map removePunc contents
    where removePunc list = [x | x <- list, not (x `elem` ":,")]    

-- Pokiaľ narazíme na Node, je potrebné nájsť indexy jeho potomkov.
-- Potomkovia budú mať zanorenie o 1 vyššie ako rodič - hodnota ind.
-- Pôvodný zoznam zoznamov stringov - (x:xs)
-- Pomocná funckia level element extrahuje hodnotu zanorenia aktuálneho prvku, ktorá je vždy na konci
-- Pokiaľ sa hodnota nezhoduje, inkrementujeme počítač indexu prvku v zozname zoznamov stringov.
-- Až nájdeme oba výskyty, vrátime indexy oboch prvkov ako dvojicu (first, second)
-- TODO -- FIRST NOT USED, REFACTOR
findNestLevel _ [] = error "Not found findNestLevel, tree probably missing Leafs"
findNestLevel ind (x:xs) = helper ind (x:xs) (0,0) False
                        where helper ind (x:xs) (first, second) firstFound
                                | ind /= (level x) && firstFound == False = helper ind xs (first + 1, second + 1) False
                                | ind == (level x) && firstFound == False = helper ind xs (first, second + 1) True
                                | ind /= (level x) && firstFound == True = helper ind xs (first, second + 1) True
                                | ind == (level x) && firstFound == True = (first, second)
                                        where level element =  (read (last element) :: Float)                         

-- Tvorba stromovej štruktúry
-- Vstup je zoznam zoznamov stringov - (x:xs)
-- Ak je prvok Node, vytvoríme hodnotu typu Node, pričom do konštruktorov jeho potomkov
-- predáme patričné splitnutý zoznam podľa hodnôt zanorení.                                
parseTree (x:xs) 
    | (head x) == "Node" = Node (read (x !! 1) :: Int) (read (x !! 2) :: Float) (parseTree (take (getSndFromFind (x:xs)) xs)) (parseTree (drop (getSndFromFind (x:xs)) xs))
    | (head x) == "Leaf" && (not (null xs)) = error "Not a tree" 
    | (head x) == "Leaf" = Leaf (x !! 1) 
    | otherwise = error "COSKA STRASNE NEDOBRE"
    where getSndFromFind (x:xs) = snd (findNestLevel ((level x)+1) xs) 
            where level element =  (read (last element) :: Float)      
  
-- Parsovanie hodnôt pre klasifikáciu  
-- Pôvodný string rozdelíne podla znakov nového riadku a rozdelíme
-- Následne vytvoríme nový zoznam, kde budú pôvodné hodnoty typu Float
parseClassificationValues input = map makeFloat $ (map (splitOn (",")) $ lines input)
                                        where makeFloat list = [read x :: Float | x <- list ]

-- Parsovanie vstupnych argumentov

                              
                                                                    