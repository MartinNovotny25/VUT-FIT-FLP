import System.IO (readFile)
import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Data.List 
import Data.List.Split
import Data.Function (on)
import qualified Data.Text as T


main = do 
    contents <- readFile "sample-input.txt"
    putStrLn "\n"
    putStrLn "Original"
    print contents
    putStrLn "\n"

    putStrLn "Original with removed national characters"
    let modifiedContents = T.unpack (T.replace (T.pack "\9532\214") (T.pack "r") (T.pack contents))
    let modifiedContents2 = T.unpack (T.replace (T.pack "\9500\161") (T.pack "i") (T.pack modifiedContents))
    print $ modifiedContents2
    putStrLn "\n"

    putStrLn "Original worded and concatenated:"
    print $  splitInputByWords modifiedContents2
    putStrLn "\n"
    putStrLn "Removed (:)"
    print $ map (removeStrednik) $ splitInputByWords modifiedContents2

    --putStrLn "Removed national characters"
    --print $ map (subsChar) $ map (removeStrednik) $ splitInputByWords contents


  
-- Parse text --------------------------
parseInput contents = (map (split (dropDelims $ oneOf ":")) $ concat (map words $ lines contents))

splitInputByWords list = (map words $ lines list)

removeStrednik contents = map removePunc contents
    where removePunc list = [x | x <- list, not (x `elem` ":,")] 

      