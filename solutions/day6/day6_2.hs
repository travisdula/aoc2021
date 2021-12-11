import Data.List.Split

main = interact $ show . solve . parse . map read . splitOn ","

parse :: [Int] -> [Int]
parse xs = map f [0..8]
    where f x = length $ filter (x==) xs

day :: [Int] -> [Int]
day [zero,one,two,three,four,five,six,seven,eight] = [one, two, three, four, five, six, zero+seven, eight, zero]

solve :: [Int] -> Int
solve xs = sum $ days xs !! 256
    where days = iterate day
