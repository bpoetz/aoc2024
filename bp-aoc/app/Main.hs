module Main (main) where
import Data.List

main :: IO ()
main = do 
        contents <- readFile "../day_2.txt" 
        print . safetyReport . lines $ contents

pairs xs = zip xs (tail xs)
-- returns 1 if the row is successful
check :: [String] -> Integer
check xs  
      | any bigJump diffs = 0
      | any noDirection diffs = 0
      | any differentDirection diffs = 0 
      | otherwise = 1
      where diffs = map (\(x,y) -> (read x::Integer) - (read y::Integer)) (pairs xs)
            differentDirection = (\x -> ((head diffs) > 0) /= (x>0))
            noDirection = (\x -> (x == 0))
            bigJump = (\x -> (abs x) > 3)
     

safetyReport xs =  foldr (+) 0 $ (map (\x -> check (words x)) xs)
   
