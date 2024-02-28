import System.IO (readFile)
import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Data.List 
import Data.List.Split
import Data.Function (on)
import qualified Data.Text as T

-- Tree structure
--data Tree a b  = EmptyTree | Node a b (Tree a b) (Tree a b)| Leaf a deriving (Show)
--data Tree a b = Leaf a | Node a b (Tree) (Tree) deriving (Show, Eq)
data Tree = Leaf String | Node Int Float (Tree) (Tree) deriving (Show, Eq)



--treeInsert xs treeStruct 
--    | treeStruct == EmptyTree  && (head xs) == "Node" = Node (treeInsert (drop 1 xs))



main = do 
    contents <- readFile "sample-input.txt"
    putStrLn "\n"
    putStrLn "Input:"
    print contents
    putStrLn "\n"
    putStrLn "Parsed Tree structure:"
    print $ parseTree $ map (removeStrednik) $ splitInputByWords contents
    putStrLn "\n"


  
-- Parse text -------------------------
addNestLevel list =  zipWith (\x y -> x ++ ", " ++ show y) (lines list) (map countSpaces (lines list))

splitInputByWords list = (map words $ addNestLevel list)

removeStrednik contents = map removePunc contents
    where removePunc list = [x | x <- list, not (x `elem` ":,")]    


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