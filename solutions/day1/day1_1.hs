main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve xs = sum $ map isGreater $ zip (tail xs) xs
    where isGreater (x, y) = if x > y then 1 else 0
