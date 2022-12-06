import Control.Exception (assert)
import Data.Either
import Data.Foldable
import Data.List
import Control.Lens

group5 [] = []
group5 lines = assert (length lines `mod` 5 == 0) (take 5 lines : group5 (drop 5 lines))

newtype Board = Board {rowcols :: [[Int]]}
  deriving (Show, Eq)

makeBoard :: [[Int]] -> Board
makeBoard lines = Board (lines <> transpose lines)

pickNum :: Int -> Board -> (Bool, Board)
pickNum n (Board b) = (any null next, Board next)
  where
    next = map (filter (/= n)) b

step :: [Board] -> Int -> Either (Int, Board) [Board]
step boards n = if null next && length boards == 1 then Left (n, snd $ head intermed) else Right next
  where
    intermed = map (pickNum n) boards
    next = map snd $ filter (not . fst) intermed

main = do
  input <- readFile "4.in"
  let int_lines = map (map read . (filter (/= "") . words)) $ filter (/= "") $ lines $ map (\c -> if c == ',' then ' ' else c) input :: [[Int]]
  let bingo_nums = head int_lines
  let boards = map makeBoard $ group5 $ tail int_lines
  let winner = foldlM step boards bingo_nums
  print winner
  let sum_remaining = sum $ concat $ take 5 $ rowcols $ winner ^?! (_Left._2)
  print $ sum_remaining * winner ^?! (_Left._1)
