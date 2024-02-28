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



testStringList (x:xs)
    | xs == [] = if (x == ["Node"]) then [["Node END"]] else [["Leaf END"]]
    | (head x) == "Node" = ([(head x) ++ ")))"] ++ (tail x)) : testStringList xs
    | (head x) == "Leaf" = ([(head x) ++ "!!!"] ++ (tail x)) : testStringList xs    

countSpaces (x:xs) = helper (x:xs) 0
                        where helper (x:xs) count 
                                | x == ' ' = helper xs (count + 1)
                                | otherwise = count    