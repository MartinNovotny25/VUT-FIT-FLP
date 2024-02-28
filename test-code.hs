import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Data.List 
import Data.List.Split
import Data.Function (on)
import qualified Data.Text as T

data Tree = Leaf | Node (Tree) (Tree) deriving (Show, Eq)
data Side = MyLeft | MyRight

level element =  (read (last element) :: Int)  

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
    | (head x) == "N" = Node (parseTree (take (getSndFromFind (x:xs)) xs)) (parseTree (drop (getSndFromFind (x:xs)) xs))
    | (head x) == "L" && (not (null xs)) = error "Not a tree" 
    | (head x) == "L" = Leaf
    | otherwise = error "COSKA STRASNE NEDOBRE"



        
 take (getSndFromFind [["N", "0"], ["L", "1"], ["L", "1"], ["N", "1"]]) [["L", "1"], ["L", "1"], ["N", "1"]]

-- Funkcne
--testStringList (x:xs)
--    | xs == [] = if (x == ["N"]) then [["N"]] else [["O"]]
--    | x == ["N"] = x : testStringList xs
--    | otherwise = x : testStringList xs

-- [["Node","0","5.5"],["Node","1","3.3"],["Leaf","X"],["Leaf","Y"],
-- --["Node","2","3.0"],["Leaf","B"],["Leaf","C"]]

testStringList (x:xs)
    | xs == [] = if (x == ["Node"]) then [["Node END"]] else [["Leaf END"]]
    | (head x) == "Node" = ([(head x) ++ ")))"] ++ (tail x)) : testStringList xs
    | (head x) == "Leaf" = ([(head x) ++ "!!!"] ++ (tail x)) : testStringList xs    