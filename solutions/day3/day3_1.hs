import Data.Char
main = interact $ show . solve . (map $ map digitToInt) . lines

readBinary :: [Int] -> Int
readBinary = foldl (\x y -> x*2 + y) 0

solve :: [[Int]] -> Int
solve xs = gamma * epsilon
    where 
        common = map (div (length xs) 2 <) $ foldl1 (zipWith (+)) xs
        gamma = readBinary $ map fromEnum common
        epsilon = readBinary . map fromEnum $ map not common
