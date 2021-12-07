main = interact $ show . solve . map words . lines

commandToChange :: [String] -> [Int]
commandToChange (s:n:[]) = case s of
    "forward" -> [read n, 0, 0]
    "down"    -> [0, read n, 0]
    "up"      -> [0, negate $ read n, 0]


-- [horizontal, aim, depth]
coordinates :: [[String]] -> [Int]
coordinates xs = foldl go [0,0,0] $ map commandToChange xs 
    where
        go (a:b:c:[]) (d:e:f:[]) = [a+d,b+e,c+d*b]

solve :: [[String]] -> Int
solve xs = (\x -> f x * t x) $ coordinates xs
    where
        f = head
        t = head . tail . tail
