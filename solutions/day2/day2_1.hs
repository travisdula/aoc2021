main = interact $ show . solve . map words . lines

commandToChange :: [String] -> (Int, Int)
commandToChange (s:n:[]) = case s of
    "forward" -> (read n, 0)
    "down"    -> (0, read n)
    "up"      -> (0, negate $ read n)

tupleSum t u = (fst t + fst u, snd t + snd u)

coordinates xs = foldl tupleSum (0,0) $ map commandToChange xs 

solve :: [[String]] -> Int
solve xs = (\x -> fst x * snd x) $ coordinates xs
