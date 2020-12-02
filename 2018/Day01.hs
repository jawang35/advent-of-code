module Day1 where

import Data.Maybe
import qualified Data.Set as S

parseChange :: (Integral a, Read a) => [Char] -> a
parseChange ('+':xs) = read xs
parseChange xs       = read xs

findDuplicate :: (Num a, Ord a) => [a] -> Maybe a
findDuplicate xs = findDuplicate' S.empty 0 xs where
    findDuplicate' xset xacc (x:xs) = if xacc `S.member` xset
                                      then Just xacc
                                      else findDuplicate' (S.insert xacc xset) (xacc + x) xs

main :: IO ()
main = do
    changes <- map parseChange . lines <$> readFile "2018/sampleinputs/day01.txt"
    let part1 = sum changes
    let part2 = fromJust $ findDuplicate $ cycle changes
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
