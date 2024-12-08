module Main (main) where
import Data.List
import qualified Data.Map as M

main :: IO ()
main = do 
        contents <- readFile "../day_1.txt" 
        print . similarity . transform . words $ contents

bifurcate (x, index) acc | even index = ((read x::Integer) : (fst acc), (snd acc))
                         | not $ even index = ((fst acc), (read x::Integer) :(snd acc))
        
   

transform xs =
    foldr bifurcate ([],[]) (zip xs [0..])
    
calculateDistance (xs, ys) =
    foldr (+) 0 $ map (\(x,y) -> abs (x - y)) $ zip (sort xs) (sort ys)

similarity (xs, ys) = 
    let simMap = foldr addToSimilarityMap M.empty ys
    in foldr (+) 0 $ map (lookupSimilarity simMap) xs


addToSimilarityMap k = 
  M.insertWith (+) k 1


lookupSimilarity dict k  
  | M.member k dict= k * dict M.! k
  | M.notMember k dict = 0
