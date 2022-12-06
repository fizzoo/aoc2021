{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.List
import qualified Data.Map as M
import Data.Maybe

data Board = B
  { _board :: M.Map (Int, Int) Int,
    _height, _width :: Int
  }
  deriving (Show)

makeLenses ''Board

twoDIndex h w = [(y, x) | y <- ys, x <- xs]
  where
    xs = [0 .. w -1]
    ys = [0 .. h -1]

loadBoard lines = B b h w
  where
    w = length $ head lines
    h = length lines
    b = M.fromList $ zip (twoDIndex h w) (map (\c -> read [c] :: Int) $ concat lines)

lookupOrInf b y x = fromMaybe 99999999 (M.lookup (y, x) b)

isLowPoint b y x = c < f (y + 1) x && c < f (y -1) x && c < f y (x + 1) && c < f y (x -1)
  where
    f = lookupOrInf b
    c = f y x

lowPoints :: Board -> [((Int, Int), Int)]
lowPoints b = mapMaybe f (twoDIndex (b ^. height) (b ^. width))
  where
    m = b ^. board
    f (y, x) = if isLowPoint m y x then Just ((y, x), lookupOrInf m y x) else Nothing

findBasin :: Board -> [(Int, Int)] -> [(Int, Int)] -> Int
findBasin b visited [] = 0
findBasin b visited (c : stack) =
  if c `elem` visited || isNothing looked
    then findBasin b visited stack
    else 1 + findBasin b (c : visited) (new ++ stack)
  where
    m = b ^. board
    looked = M.lookup c m
    (y, x) = c
    cur_height = lookupOrInf m y x
    new = [nc | nc <- [(y + 1, x), (y -1, x), (y, x + 1), (y, x -1)], uncurry (lookupOrInf m) nc < 9]

findBasinSize b y x = findBasin b [] [(y, x)]

runFile file = do
  b <- loadBoard . lines <$> readFile file
  let lows = lowPoints b
  print lows
  let sizes = uncurry (findBasinSize b) . fst <$> lows
  print $ product . take 3 . reverse . sort $ sizes
