import System.IO (readFile)
import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Data.List 
import Data.List.Split
import Data.Function (on)
import qualified Data.Text as T

-- Tree structure
--data Tree a b  = EmptyTree | Node a b (Tree a b) (Tree a b)| Leaf a deriving (Show)
data Tree = EmptyTree | Leaf | Node (Tree) (Tree) deriving (Show, Eq)

--treeInsert xs treeStruct 
--    | treeStruct == EmptyTree  && (head xs) == "Node" = Node (treeInsert (drop 1 xs))



main = do 
    contents <- readFile "sample-input.txt"
    putStrLn "\n"
    putStrLn "Original"
    print contents
    putStrLn "\n"
    putStrLn "Original worded and concatenated:"
    print $  splitInputByWords contents
    putStrLn "\n"
    putStrLn "Removed (:)"
    print $ map (removeStrednik) $ splitInputByWords contents


  
-- Parse text --------------------------
parseInput contents = (map (split (dropDelims $ oneOf ":")) $ concat (map words $ lines contents))

splitInputByWords list = (map words $ lines list)

removeStrednik contents = map removePunc contents
    where removePunc list = [x | x <- list, not (x `elem` ":,")] 

--addBrackets (x:xs) acc right 
--    | x == "Node" = "(" : x : (addBrackets xs (acc+1) False)
--    | x == "Leaf" && right == True  = (x : (replicate acc ")")) : (addBrackets xs 0 False)
--    | x == "Leaf" && right == False = (x : (addBrackets acc True))


-- Funkcia prejde zoznam stringov (zoznam zoznamov charakterov) a ak bude prvy znak N alebo L tak sa urobi akcia
testStringList (x:xs)  
    | null xs           = ["L" : ")" : []]
    | (head x) == "N"   = ("(" : "N" : []) : testStringList xs
    | (head x) == "L"   = (")" : "L" : []) : testStringList xs

testString (x:xs)
    | null xs = [x]
    | x == 'k' =  '(' : x : (testString xs)
    | otherwise = x :  (testString xs)
  