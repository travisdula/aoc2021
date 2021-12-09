import Data.List.Split
import Data.List

type Board = [[(Int, Bool)]]

main = interact $ show . solve . parser . lines


parser :: [String] -> ([Int], [Board])
parser xs = (numsDrawn, boards)
    where
        numsDrawn = map read . splitOn "," . head $ xs
        numberBoards = map tail . chunksOf 6 . map (map read . words) . tail $ xs
        boards = map (map (map (\x -> (x, False)))) numberBoards

isSolved :: Board -> Bool
isSolved board = vertical || horizontal
    where
        vertical = any (all snd) $ transpose board
        horizontal = any (all snd) board

mark :: Int -> Board -> Board
mark num = map (map (\x -> (fst x, fst x ==num || snd x))) 

score :: Int -> Board -> Int
score lastNumber board = lastNumber * total
    where total = sum . map fst . filter (not . snd) . concat $ board

solve :: ([Int], [Board]) -> Int
solve (current:nums, boards) =
    if null unsolved
    then score current (head solved)
    else solve (nums, unsolved)
    where
        next = map (mark current) boards
        (solved, unsolved) = partition isSolved next
