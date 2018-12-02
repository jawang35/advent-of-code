module Day2 where

import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

charCounts :: (Ord a) => [a] -> Set Int
charCounts = Set.fromList . map length . group . sort

checksum :: (Ord a) => [[a]] -> Int
checksum ids = doubles * triples where
    idCharCounts = map charCounts ids
    doubles = length $ filter (2 `Set.member`) idCharCounts
    triples = length $ filter (3 `Set.member`) idCharCounts

common :: (Eq a) => [a] -> [a] -> [a]
common id1 id2 = map fst $ filter (\(a, b) -> a == b) $ zip id1 id2

findCorrectBoxId :: (Eq a) => [[a]] -> Maybe [a]
findCorrectBoxId []       = Nothing
findCorrectBoxId (id:ids) = if correct == Nothing
                            then findCorrectBoxId ids
                            else correct where
    correctLength = length id - 1
    commons       = map (common id) ids
    correct       = find ((== correctLength) . length) commons

main :: IO ()
main = do
    ids <- lines <$> readFile "sampleinputs/day2.txt"
    let part1 = checksum ids
    let part2 = fromJust $ findCorrectBoxId ids
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ part2
