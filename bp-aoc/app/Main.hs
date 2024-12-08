module Main (main) where
import Data.List

main :: IO ()
main = do 
        contents <- readFile "../day_2.txt" 
        print . part2SafetyReport . lines $ contents

pairs xs = zip xs (tail xs)
-- returns 1 if the row is successful
check xs  
      | any bigJump diffs = 0
      | any noDirection diffs = 0
      | any differentDirection diffs = 0 
      | otherwise = 1
      where diffs = map (\(x,y) -> (read x::Integer) - (read y::Integer)) (pairs xs)
            differentDirection = (\x -> ((head diffs) > 0) /= (x>0))
            noDirection = (\x -> (x == 0))
            bigJump = (\x -> (abs x) > 3)


multicheck xs
      | (check xs) == 1 = 1
      | otherwise = bruteForce xs
      
allButPosition 0 xs = tail xs
allButPosition idx xs | length xs == idx = take (idx) xs  
                      | otherwise = take (idx) xs ++ drop (idx+1) xs


safetyReport xs =  foldr (+) 0 $ (map (\x -> check (words x)) xs)

part2SafetyReport xs = foldr (+) 0 $ (map (\x -> multicheck (words x)) xs)
   
bruteForce xs 
   | any (\x -> check x == 1) [allButPosition idx xs | idx<-[0..listSize] ] = 1
   | otherwise = 0
   where listSize = length xs-1


