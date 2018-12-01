module Day1 where

import Data.Maybe
import qualified Data.Set as Set

parseChange :: (Integral a, Read a) => [Char] -> a
parseChange ('+':xs) = read xs
parseChange xs       = read xs

findDuplicate :: (Num a, Ord a) => [a] -> Maybe a
findDuplicate xs = findDuplicate' Set.empty 0 xs where
    findDuplicate' xset xacc (x:xs) = if xacc `Set.member` xset
                                      then Just xacc
                                      else findDuplicate' (Set.insert xacc xset) (xacc + x) xs

main :: IO ()
main = do
    changes <- map parseChange . lines <$> readFile "inputs/day1.txt"
    let part1 = sum changes
    let part2 = fromJust $ findDuplicate $ cycle changes
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
