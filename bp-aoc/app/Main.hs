module Main (main) where

main :: IO ()
main = do 
        contents <- readFile "../day_1.txt" 
        print . splitCols . words $ contents


bifurcate (x, index) acc | even index = (x: (fst acc), (snd acc))
                         | not $ even index = ((fst acc), x:(snd acc))
        
   

splitCols xs=
    foldr bifurcate ([],[]) (zip xs [0..])
