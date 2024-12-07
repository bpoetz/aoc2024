module Main (main) where
import Data.List

main :: IO ()
main = do 
        contents <- readFile "../day_1_test.txt" 
        print . calculateDistance . transform . words $ contents

bifurcate (x, index) acc | even index = ((read x::Integer) : (fst acc), (snd acc))
                         | not $ even index = ((fst acc), (read x::Integer) :(snd acc))
        
   

transform xs=
    foldr bifurcate ([],[]) (zip xs [0..])
    
calculateDistance (xs, ys) =
    foldr (+) 0 $ map (\(x,y) -> abs (x - y)) $ zip (sort xs) (sort ys)
