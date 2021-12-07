import Data.Char
import Debug.Trace

main = interact $ show . solve . (map $ map digitToInt) . lines

commonB xs = map (minMajority <=) $ foldl1 (zipWith (+)) xs
    where minMajority = div (length xs) 2 + mod (length xs) 2
common xs = map fromEnum $ commonB xs
uncommon xs = map fromEnum . map not $ commonB xs

readBinary :: [Int] -> Int
readBinary = foldl (\x y -> x*2 + y) 0

bitReduce :: ([[Int]] -> [Int]) -> Int -> [[Int]] -> [Int]
bitReduce _ _ (x:[]) = x
bitReduce rater i nums = bitReduce rater (i+1) reduced
    where
        mask = rater nums
        reduced = filter (\x -> (x !! i) == (mask !! i))  nums

solve :: [[Int]] -> Int
solve xs = oxygen * scrubber
    where 
        oxygen = readBinary $ bitReduce common 0 xs
        scrubber = readBinary $ bitReduce uncommon 0 xs
