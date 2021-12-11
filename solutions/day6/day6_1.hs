import Data.List.Split

main = interact $ show . solve . map read . splitOn ","

day :: Int -> [Int]
day 0 = [6, 8]
day n = [n - 1]

solve :: [Int] -> Int
solve xs = length $ days xs !! 80
    where days = iterate $ concatMap day
