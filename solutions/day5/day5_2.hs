import Data.Char
import Data.List
import Data.List.Split

type Line = [Int]

main = interact $ show . solve . map parseLine . lines

parseLine :: String -> Line
parseLine = map read . wordsBy (not . isDigit)

toPoints :: Line -> [(Int, Int)]
toPoints [x1,y1, x2,y2] = zip horizontal vertical
    where
        horizontal' = if x1 <= x2 then [x1..x2] else reverse [x2..x1]
        vertical' = if y1 <= y2 then [y1..y2] else reverse [y2..y1]
        l = max (length horizontal') (length vertical')
        horizontal = if length horizontal' == l then horizontal' else replicate l (head horizontal')
        vertical = if length vertical' == l then vertical' else replicate l (head vertical')

solve :: [Line] -> Int
solve xs = overlaps
    where
        points = concatMap toPoints xs
        overlaps = length . filter (\x -> 1 < length x) . group . sort $ points
