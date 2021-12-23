import Data.List
import Data.List.Split

main = interact $ show . solve . map (map words . splitOn "|") . lines

solve :: [[[String]]] -> Int
solve = sum . map (length . filter is1478 . head . tail)
    where is1478 x = length x `elem` [2,3,4,7]
