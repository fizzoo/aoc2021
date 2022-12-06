{-# LANGUAGE MultiWayIf #-}

import qualified Data.Map as M
import qualified Data.Text as T

parseAllInts :: String -> [Int]
parseAllInts s = map read $ filter (not . null) $ map T.unpack $ T.split (\x -> x `notElem` ['0' .. '9']) (T.pack s)

parseLine :: String -> (Int, Int, Int, Int)
parseLine s = case ints of
  [x, y, z, w] -> (x, y, z, w)
  _ -> error $ show ints
  where
    ints = parseAllInts s

type Board = M.Map (Int, Int) Int

range :: Int -> Int -> [Int]
range x y = if x == y then [] else [x, x + signum (y - x) .. y]

makeBoard (x1, y1, x2, y2) =
  if
      | x1 == x2 -> makey
      | y1 == y2 -> makex
      | otherwise -> makexy
  where
    makex = M.fromList (zip (zip (range x1 x2) (repeat y1)) (repeat 1))
    makey = M.fromList (zip (zip (repeat x1) (range y1 y2)) (repeat 1))
    makexy = M.fromList (zip (zip (range x1 x2) (range y1 y2)) (repeat 1))

runFile file = do
  input <- readFile file
  let combined_board = foldl (M.unionWith (+)) M.empty $ map (makeBoard . parseLine) $ lines input
  print $ length $ filter (>= 2) $ map snd $ M.toList combined_board

main = do
  runFile "5test.in"
  runFile "5.in"
