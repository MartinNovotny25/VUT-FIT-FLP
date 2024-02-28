import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Data.List 
import Data.List.Split
import Data.Function (on)
import qualified Data.Text as T

--testStringList (x:xs)  
--    | null xs           = [([x] ++ ")")] : []
--    | [[x]] == ["N"]      = [("(" ++ [x])] : testStringList xs
--    | [[x]] == ["L"]      = [(")" ++ [x])] : testStringList xs

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