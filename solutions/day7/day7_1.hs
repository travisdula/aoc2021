import Data.List.Split
import Data.List

main = interact $ show . solve . map read . splitOn ","

medianSorted :: [Int] -> Int
medianSorted xs = xs !! (length xs `div` 2)

solve :: [Int] -> Int
solve xs = sum $ map (abs . subtract median) xs
    where median = medianSorted $ sort xs
