module Main (main) where
import Data.List

main :: IO ()
main = do 
        contents <- readFile "../day_2.txt" 
        print . part2SafetyReport . lines $ contents

-- for some list, return a 2-tuple set of all neighbours
pairs xs = zip xs (tail xs)

-- check for safe reactor conditions
-- we are checking the inverse of what was specified, maybe we could do this in a less confusing way?
-- TODO: does it matter that we are iterating up to 3x? Can we get there faster with a combinator?
-- TODO: Parsing logic (read x::Integer) does not belong here, learn to use the parsing library James used in 
-- their solution
check xs  
      | any bigJump diffs = 0
      | any noDirection diffs = 0
      | any differentDirection diffs = 0 
      | otherwise = 1
      where diffs = map (\(x,y) -> (read x::Integer) - (read y::Integer)) (pairs xs)
            differentDirection = (\x -> ((head diffs) > 0) /= (x>0))
            noDirection = (\x -> (x == 0))
            bigJump = (\x ->  (abs x) >3)

-- checks the input list for adherence to criteria in check, otherwise
-- we try removing each element 
multicheck xs
      | (check xs) == 1 = 1
      | otherwise = bruteForce xs

-- for some list, we return that list less the index specified      
allButPosition 0 xs = tail xs
allButPosition idx xs = take (idx) xs ++ drop (idx+1) xs

-- searches all combinations of lists until it finds 
-- a safe one, I think. TODO: validate this assumption
bruteForce xs 
   | any (\x -> check x == 1) [allButPosition idx xs | idx<-[0..listSize] ] = 1
   | otherwise = 0
   where listSize = length xs-1



part1SafetyReport xs =  foldr (+) 0 $ (map (\x -> check (words x)) xs)

part2SafetyReport xs = foldr (+) 0 $ (map (\x -> multicheck (words x)) xs)
   

