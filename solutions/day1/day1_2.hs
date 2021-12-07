main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve xs = sum $ map isGreater . zip combined $ [head combined] ++ combined
    where
        isGreater (x, y) = if x > y then 1 else 0
        a = xs
        b = tail xs
        c = drop 2 xs
        combined :: [Int]
        combined = map (\(x, y, z) -> x+y+z) $ zip3 a b c
