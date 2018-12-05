module Day4 where

import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V

data Record = BeginsShift {guardId :: Int}
            | FallsAsleep {minute :: Int}
            | WakesUp     {minute :: Int}
            deriving (Show)

parseRecord :: [Char] -> Record
parseRecord rstring
    | "falls asleep" `isInfixOf` rstring = FallsAsleep minute
    | "wakes up" `isInfixOf` rstring     = WakesUp minute
    | otherwise                          = BeginsShift guardId
    where minute  = read $ take 2 $ drop 15 rstring
          guardId = read $ takeWhile ((/=) ' ') $ drop 1 $ dropWhile ((/=) '#') rstring

updateMinuteCount :: (Num a) => Int -> Int -> Vector a -> Vector a
updateMinuteCount start end minuteCount = V.map update $ V.zip indices minuteCount where
    update (i, m) = if start <= i && i < end then m + 1 else m
    indices       = V.generate (V.length minuteCount) id

buildSleepLog :: Int -> Maybe Int -> Map Int (Int, Vector Int) -> [Record] -> Map Int (Int, Vector Int)
buildSleepLog _ _ rmap []                     = rmap
buildSleepLog guardId asleepSince rmap (r:rs) = case r of BeginsShift newGuardId -> buildSleepLog newGuardId Nothing rmap rs
                                                          FallsAsleep minute     -> buildSleepLog guardId (Just minute) rmap rs
                                                          WakesUp minute         -> buildSleepLog guardId Nothing (M.insert guardId (updatedLog minute) rmap) rs
    where minutesAsleep currentMinute = currentMinute - (fromJust asleepSince)
          updatedLog currentMinute    = case M.lookup guardId rmap of Nothing                -> (minutesAsleep currentMinute, updateMinuteCount (fromJust asleepSince) currentMinute $ V.replicate 60 0)
                                                                      Just (total, minutes)  -> (minutesAsleep currentMinute + total, updateMinuteCount (fromJust asleepSince) currentMinute minutes)

maximumIndex :: (Ord a) => Vector a -> Int
maximumIndex xs = let maxValue = V.maximum xs in fromJust $ V.findIndex ((==) maxValue) xs

main :: IO ()
main = do
    records <- map parseRecord . sort . lines <$> readFile "2018/sampleinputs/day4.txt"
    let sleepLog = buildSleepLog 0 Nothing M.empty records
    let (sleepingBeauty1, log1) = head $ sortBy (flip compare `on` (fst . snd)) $ M.toList sleepLog
    let (sleepingBeauty2, log2) = head $ sortBy (flip compare `on` (maximum . snd . snd)) $ M.toList sleepLog
    let part1 = sleepingBeauty1 * (maximumIndex $ snd log1)
    let part2 = sleepingBeauty2 * (maximumIndex $ snd log2)
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
