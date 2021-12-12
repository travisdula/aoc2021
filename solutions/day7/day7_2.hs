import Data.List.Split
import Data.List

main = interact $ show . solve . map read . splitOn ","

cost :: [Int] -> Int -> Int
cost xs pos = sum . map (fuel . abs . subtract pos) $ xs
    where fuel n = (n * (n + 1)) `div` 2

solve :: [Int] -> Int
solve xs = minimum costs
    where costs = map (cost xs) [(minimum xs)..(maximum xs)]
