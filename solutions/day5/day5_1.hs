import Data.Char
import Data.List
import Data.List.Split

type Line = [Int]

main = interact $ show . solve . map parseLine . lines

parseLine :: String -> Line
parseLine = map read . wordsBy (not . isDigit)

toPoints :: Line -> [(Int, Int)]
toPoints [x1,y1, x2,y2]= if x1 == x2
    then vertical
    else horizontal
    where
        vertical = map (\y -> (x1, y)) [min y1 y2 .. max y1 y2]
        horizontal = map (\x -> (x, y1)) [min x1 x2 .. max x1 x2]

cleanPoints = filter (\[x1,y1,x2,y2] -> x1==x2 || y1==y2)
           
solve :: [Line] -> Int
solve xs = overlaps
    where
        points = concatMap toPoints $ cleanPoints xs
        overlaps = length . filter (\x -> 1 < length x) . group . sort $ points
