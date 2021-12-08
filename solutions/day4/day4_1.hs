import Data.List.Split

type Board = [[(Int, Bool)]]

main = interact $ show . solve . parser . lines


parser :: [String] -> ([Int], [Board])
parser xs = (numsDrawn, boards)
    where
        numsDrawn = map read . splitOn "," . head $ xs
        numberBoards = map tail . chunksOf 6 . map (map read . words) . tail $ xs
        boards = map (map (map (\x -> (x, False)))) numberBoards

-- thanks internet for this one
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

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
solve (nums, boards) =
    if not $ null solved
    then score (head nums) (head solved)
    else solve(tail nums, next)
    where
        next = map (mark (head nums)) boards
        solved = filter isSolved next
