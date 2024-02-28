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
    putStrLn "With added nested levels"
    print $ addNestLevel contents
    putStrLn "\n"
    putStrLn "Original worded and concatenated:"
    print $  splitInputByWords contents
    putStrLn "\n"
    putStrLn "Removed (:)"
    print $ map (removeStrednik) $ splitInputByWords contents


  
-- Parse text -------------------------
addNestLevel list =  zipWith (\x y -> x ++ ", " ++ show y) (lines list) (map countSpaces (lines list))

splitInputByWords list = (map words $ addNestLevel list)

removeStrednik contents = map removePunc contents
    where removePunc list = [x | x <- list, not (x `elem` ":,")] 
   
countSpaces (x:xs) = helper (x:xs) 0
                        where helper (x:xs) count 
                                | x == ' ' = helper xs (count + 1)
                                | otherwise = count    
  