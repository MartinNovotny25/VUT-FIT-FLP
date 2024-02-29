-- FLP Proj 1 - Klasifikačné stromy
-- Autor: Martin Novotný Mlinárcsik
-- xlogin: xnovot1r
-- 29/02/2024
-- VUT FIT

import System.IO (readFile)
import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Data.List 
import Data.List.Split
import Data.Function (on)
import qualified Data.Text as T

data Tree = Leaf String | Node Int Float (Tree) (Tree) deriving (Show, Eq)

-- Main ----------------------------------------------------------------------
main = do 
    contents <- readFile "sample-input.txt"
    --values <- readFile ""
    putStrLn "\n"
    putStrLn "Input:"
    print contents
    putStrLn "\n"
    putStrLn "Parsed Tree structure:"
    print $ parseTree $ map (removeFluff) $ splitInputByWords contents
    putStrLn "\n"

    print $ "Klasifikacia"
    putStrLn $ classifyAll [[2.4, 1.3], [6.1, 0.3]] $ parseTree $ map (removeFluff) $ splitInputByWords contents


-- Klasifikácia
classifyAll (x:xs) tree
    | xs == [] = classification x tree
    | otherwise = (classification x tree) ++ "\n" ++ classifyAll (xs) tree 
classification (x:xs) (Node a b left right) 
    | x <= b = classification xs left
    | otherwise = classification xs right
classification x (Leaf a) = a




  
-- Parse text ------------------------------------------------------------------
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
  