-- FLP Proj 1 - Klasifikačné stromy
-- Autor: Martin Novotný Mlinárcsik
-- xlogin: xnovot1r
-- 29/02/2024
-- VUT FIT

import System.Environment 
import Data.List 
import Data.List.Split
import Data.Maybe

-- Datový typ strom
-- Leaf v sebe obsahuje triedu
-- Node v sebe obsahuje index priznaku, treshold hodnotu a dvoch potomkov
-- Struktura prebrana z Learn You a Haskell
-- https://learnyouahaskell.github.io/making-our-own-types-and-typeclasses.html#recursive-data-structures

data Tree = Leaf String | Node Int Float (Tree) (Tree) deriving (Eq)

-- Inštancia pre výpis stromu
instance Show Tree where
    show tree = showIndented 0 tree
      where
        showIndented :: Int -> Tree -> String
        showIndented depth (Leaf str) = replicate (depth * 2) ' ' ++ "Leaf: " ++ str
        showIndented depth (Node intVal floatVal left right) =
            replicate (depth * 2) ' ' ++
            "Node: " ++ show intVal ++ ", " ++ show floatVal ++ "\n" ++
            showIndented (depth + 1) left ++ "\n" ++
            showIndented (depth + 1) right


-- Main ----------------------------------------------------------------------
main :: IO ()
main = do 
    args <- getArgs
    case args of [] -> error "Empty"
                 "-1" : treeInput : [dataInput] -> do 
                                                    treeStr <- readFile treeInput   
                                                    dataStr <- readFile dataInput
                                                    doClass treeStr dataStr
                 "-2" : [dataInput] -> do
                                         dataStr <- readFile dataInput
                                         -- Vstup rozdelime podla stplcov, odstranime prazdne riadky
                                         --print $ (filter (/= [""])) (map (splitOn (",")) $ lines dataStr) 
                                         print $ doTrain $ (filter (/= [""])) (map (splitOn (",")) $ lines dataStr)                                
                 _ -> error "Argumenty coska nedobre"


------ Vypocet GINI index ---------------------------------------
-- prebrané z https://www.youtube.com/watch?v=_L39rN6gz7Y

-- Tvorba uzlov a listov
-- Ak na vstup príde zoznam, ktorý obsahuje rovnaké triedy, môžeme vytvoriť list
-- Ak nie, vytvoríme Node a splitujeme ďalej
doTrain :: [[String]] -> Tree
doTrain dataInput  
     | (containsSameLabels dataInput) == True = Leaf (last (head dataInput))
     | otherwise =
                -- maxIndex je počet stĺpcov
            let maxIndex = (length (dataInput !! 0))

                -- Zo vstupného listu extrahujeme páry (priznak, trieda) a z nich vytvoríme zoznamy. Tieto zoznamy zoradíme od najmenšieho po najväčšie.
                -- mapCalcAllWeights vypočíta vážené indexy pre každý deliaci threshold, map reverse otočí zoznam, pretože calcAllWeights ich počíta odzadu
                allGinis = (map reverse (map calcAllWeights $ (map makeString) $ (map (sortOn fst)) $ (map makeFloat) $ (map makePairsFromPairs (getPairs 0 maxIndex dataInput))))

                -- Vyberieme najnižšie Gini impurities
                bestGinis = map minimum allGinis
                -- Vyberieme indexy najlepších gini impurities
                indexOfBestGinis = catMaybes $ zipWith elemIndex bestGinis allGinis

                -- Vrati index najlepsieho priemeru -> vypocitam znova priemer a podla neho rozdelim vstup
                returnBestIndex =  (minimum $ zipWith makePair bestGinis indexOfBestGinis)
                -- Potrebujem vratit aj index stlpca, aby som vedel podla ktoreho splitovat
                indexOfColumn = head $ catMaybes $ [elemIndex (returnBestIndex) (zipWith makePair bestGinis indexOfBestGinis)]

                
                -- Podľa indexu najlepšieho gini impurity indexu vieme zistiť, z ktorých 2 susedných hodnôt bol vypočítaný daný priemer. Tieto 2 hodnoty vyberieme a vypočítame 
                -- priemer znova
                val1 = ((((map makeString) $ (map (sortOn fst)) $ (map makeFloat) $ (map makePairsFromPairs (getPairs 0 maxIndex dataInput))) !! indexOfColumn) !! (snd returnBestIndex)) 
                val2 = ((((map makeString) $ (map (sortOn fst)) $ (map makeFloat) $ (map makePairsFromPairs (getPairs 0 maxIndex dataInput))) !! indexOfColumn) !! ((snd returnBestIndex) +1)) 

                -- Vrati priemer podla kotreho budeme splitovat
                returnAverage = calcAverage (read (fst val1) :: Float) (read (fst val2) :: Float)

                -- Split list vrati 2 zoznami - menšie a väčšie ako threshold
                splitList = ([x | x <- dataInput, (read (x !! indexOfColumn) :: Float) <= returnAverage], [x | x <- dataInput, (read (x !! indexOfColumn) :: Float) > returnAverage])

                -- Vytvoríme uzol a rekurzivne volame doTrain na dcérskych uzloch s dielčimi zoznamami
                in (Node indexOfColumn returnAverage) (doTrain (fst splitList)) (doTrain (snd splitList))

-- Výpočet jedného váženého Gini indexu
glueTogether :: Eq b => [(String, b)] -> Int -> Float
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

-- Vráti celkový počet všetkých tried
totalNumber :: (Eq a, Eq b, Num a) => [(a, b)] -> a
totalNumber (x:xs) 
    | xs == [] = (fst x)
    | xs /= [] = (fst x) + totalNumber xs
    | otherwise = error "totalNumber - CRITICAL ERROR"

-- Výpočet Gini impurity je list
calculateGini :: (Fractional a, Eq a, Eq b) => a -> [(a, b)] -> a
calculateGini totalNumber list = 1 - (helper list totalNumber)
    where helper (x:xs) totalNumber
            | xs == [] = ((((fst x))/ ( (totalNumber)))^2)
            | xs /= [] = ((((fst x)) / ((totalNumber)))^2) + (helper xs totalNumber)     
            | otherwise = error "calculateGini - CRITICAL ERROR"     

-- listSizes ginies
calculateWeightedGini :: Fractional a => [a] -> [a] -> a -> a
calculateWeightedGini (x:xs) (y:ys) allOccurences
    | (not $ null xs) && (not $ null ys) = ((x / allOccurences)*y) + (calculateWeightedGini xs ys allOccurences)
    | (null xs) && (null ys) = ((x / allOccurences)*y)
    | (not $ null xs) && (null ys) = error "calculateWeightedgGini - ERROR, GINIES EMPTY"
    | (null xs) && (not $ null ys) = error "calculateWeightedgGini - ERROR, LIST_SIZES EMPTY"
    | otherwise = error "calculateWeightedgGini - CRITICAL ERROR"

-- Funkcie pre prevod (fst tuple) na Float a String
makeFloat :: [(String, b)] -> [(Float, b)]
makeFloat list = [((read (fst x) :: Float), snd x)| x <- list] 
makeString :: Show a => [(a, b)] -> [(String, b)]
makeString list = [((show (fst x)), snd x)| x <- list] 

-- Vytvorenie párov (príznak, trieda)
getPairs :: Int -> Int -> [[b]] -> [([b], [b])]
getPairs currentIndex maxIndex dataInput  
    | currentIndex == (maxIndex-1) = []
    | currentIndex /= (maxIndex-1) =
        let parsedColumn1 = ((parseColumns dataInput currentIndex maxIndex)) 
            parsedColumn2 = ((parseColumns dataInput (maxIndex-1) maxIndex))
        in 
            (zipWith makePair parsedColumn1 parsedColumn2) ++ (getPairs (currentIndex + 1) maxIndex dataInput)

-- Vytvorenie párov 2 listov  
makePairsFromPairs :: ([a], [b]) -> [(a, b)]                      
makePairsFromPairs ([], []) = []
makePairsFromPairs (x:xs, y:ys) = (makePair x y) : makePairsFromPairs (xs, ys)

makePair :: a -> b -> (a, b)
makePair x y = (x,y)


-- Parsovanie stĺpca 
parseColumns :: [[b]] -> Int -> Int -> [[b]]
parseColumns input index maxIndex  
    | index /= (maxIndex - 1) = map (!! index) input : parseColumns input (index+1) maxIndex
    | otherwise = (map (!! index) input) : []  

-- Prevod zo Stringu na Float
readAsFloat :: String -> Float
readAsFloat = (read :: String -> Float) 

-- Funkcia dostane list počtu labelov v liste a zvýši počítadlo o 1 ak sa label nachádza v liste
incrementListOfTuples :: (Eq b, Eq a, Num a) => [(a, b)] -> b -> [(a, b)]
incrementListOfTuples (x:xs) label  
        | (snd x) /= label && xs == [] = error "incrementListOfTuples -- label not found"
        | (snd x) /= label = x : (incrementListOfTuples xs label)
        | (snd x) == label && xs /= [] = ((fst x)+1, snd x) : xs
        | (snd x) == label && xs == [] = ((fst x)+1, snd x) : []
        | otherwise = error "incrementListOfTuples -- Critical error"

-- Výpočet všetkých vážených Gini impurities pre splitnute Nodes    
calcAllWeights :: Eq b => [(String, b)] -> [Float]
calcAllWeights inputList = iterateOver inputList ((length inputList) -1)
                    where iterateOver inputList iterations
                                | iterations /= 1 = (glueTogether inputList iterations) : (iterateOver inputList (iterations-1) )
                                | iterations == 1 = (glueTogether inputList iterations) : []

calcAverage :: Fractional a => a -> a -> a
calcAverage x y = (x+y)/2

splitByAvg :: Float -> [(String, b)] -> ([(String, b)], [(String, b)])
splitByAvg threshold list = ([x | x <- list, (read (fst x) :: Float) <= threshold], [x | x <- list, (read (fst x) :: Float) > threshold])

-- PRIKLAD VSTUPU
-- callIncrement (extractLabels [["7","N"],["12","N"],["18","Y"],["35","Y"],["38","Y"],["50","N"],["83","N"]]) (map snd [(1, "N"), (2, "Y"), (3,"N")])
-- extractLabels vytiahne unikatne páry (0, label), pricom fst sa bude inkrementovat vzdy, pokila bude rovnaky label v druhom zozname
-- druhy zoznam su hodnoty, ktore boli splitnute podla tresholdu
callIncrement :: (Eq b, Eq a, Num a) => [(a, b)] -> [b] -> [(a, b)]
callIncrement toIncrement [] = []
callIncrement toIncrement (y:[]) = incrementListOfTuples toIncrement y
callIncrement toIncrement (y:ys) = callIncrement (incrementListOfTuples toIncrement y) ys

-- True ak sa v zozname nachádzajú iba rovnaké triedy, False inak
containsSameLabels :: Eq t => [[t]] -> Bool
containsSameLabels list = helper1 list (last (head list)) 
    where   helper1 [] _ = True
            helper1 (x:xs) compareVal 
                | (last x) /= compareVal = False
                | (last x) == compareVal = helper1 xs compareVal

-- Extrahuje triedy z listu
extractLabels :: (Eq b, Num a1) => [(a2, b)] -> [(a1, b)]
extractLabels lists = helper (map snd lists) []
    where   helper [] _ = [] 
            helper (x:xs) seen
                | x `elem` seen = helper xs seen  
                | otherwise = (0, x) : helper xs (x : seen)     




------  Klasifikácia ------------------------------------------

-- Spracuje vstupy a vykoná klasifikáciu
doClass :: String -> String -> IO ()
doClass treeInput dataInput = putStrLn $ classifyAll (parseClassificationValues dataInput) $ parseTree $ map (removeFluff) $ splitInputByWords treeInput

-- classifyAll je pomocná funckia, ktorá aplikuje funckiu classification pre každý
-- zoznam v zozname hodnôť pre klasifikáciu
classifyAll :: [[Float]] -> Tree -> [Char]
classifyAll (x:xs) tree
    | xs == [] = classification x tree
    | otherwise = (classification x tree) ++ "\n" ++ classifyAll (xs) tree 

-- classification porovnáva treshold hodnotu aktuálneho uzlu so vstupom
-- Mensia rovná -> ľavý podstrom, väčšia -> pravý podstrom
-- Ak narazíme na Leaf, vrátime triedu
classification :: [Float] -> Tree -> String
classification [] (Node a b left right) = error "Ended on a node"
classification (x:xs) (Node a b left right) 
    | x <= b = classification xs left
    | otherwise = classification xs right
classification x (Leaf a) = a    



------  Parse text  ------------------------------------------------------------------
-- splitInputByWords dostane prečítaný vstupný reťazec, s pomocou addNestLevel appendne každému
-- prvku stromu jeho hĺbku zanorenia. Keďže addNestLevel vráti zoznam retazcov, pouzijeme map words
-- a kazdy reťazec v zozname rozdelíme na slová
splitInputByWords :: String -> [[String]]
splitInputByWords inputString = (map words $ addNestLevel inputString)

-- addNestLevel pridáva na koniec každého prvku stromu hĺbku  zanorenia.
-- countSpaces vráti zoznam s odpovedajúcimi hĺbkami zanorenia každého prvku.
-- Pomocou zipWith prikonkatenujeme hĺbky zanorenia každému prvku stromu.
-- Prikonkatenujeme vo formáte:

--                      originalString ++ ", " ++ show y
-- keďže v povodnom reťazci sú jednotlivé položky prvku oddelené čiarkou. Show y konvertuje
-- Int hodnotu na String. Čiarka je odstránea v následujúcich krokoch.

addNestLevel :: String -> [[Char]]                            
addNestLevel list =  zipWith (\x y -> x ++ ", " ++ show y) (lines list) (map countSpaces (lines list))
    where countSpaces (x:xs) = helper (x:xs) 0
                        where helper (x:xs) count 
                                | x == ' ' = helper xs (count + 1)
                                | otherwise = if (count == 0) then 0 else count / 2  

-- removeFluff odstráni z jednotlivých retazcov znaky ':' a ','.
removeFluff :: [[Char]] -> [[Char]]
removeFluff contents = map removePunc contents
    where removePunc list = [x | x <- list, not (x `elem` ":,")]    

-- Pokiaľ narazíme na Node, je potrebné nájsť indexy jeho potomkov.
-- Potomkovia budú mať zanorenie o 1 vyššie ako rodič - hodnota ind.
-- Pôvodný zoznam zoznamov stringov - (x:xs)
-- Pomocná funckia level element extrahuje hodnotu zanorenia aktuálneho prvku, ktorá je vždy na konci
-- Pokiaľ sa hodnota nezhoduje, inkrementujeme počítač indexu prvku v zozname zoznamov stringov.
-- Až nájdeme oba výskyty, vrátime indexy oboch prvkov ako dvojicu (first, second)
-- TODO -- FIRST NOT USED, REFACTOR
findNestLevel :: (Num a, Num b) => Float -> [[String]] -> (a, b)
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
parseTree :: [[String]] -> Tree                      
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
parseClassificationValues :: String -> [[Float]]
parseClassificationValues input = map makeFloat $ (map (splitOn (",")) $ lines input)
                                        where makeFloat list = [read x :: Float | x <- list ]
