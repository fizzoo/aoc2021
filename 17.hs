{-# LANGUAGE FlexibleContexts #-}

import Control.Lens
import Data.List
import Data.Maybe
import Text.Regex.Posix

data Target = Target {xmin, xmax, ymin, ymax :: Int} deriving Show

drag x | x > 0 = x - 1
       | x < 0 = x + 1
       | x == 0 = 0
       | otherwise = undefined

parseInput :: String -> Target
parseInput x = parseTarget $ map read $ tail $ getAllTextSubmatches $ x =~ "target area: x=(-?[0-9]+)\\.\\.(-?[0-9]+), y=(-?[0-9]+)\\.\\.(-?[0-9]+)"
  where
    parseTarget [a,b,c,d] = Target a b c d
    parseTarget _ = undefined

insideTarget :: Target -> (Int, Int) -> Bool
insideTarget t (x, y) = x >= xmin t && x <= xmax t && y >= ymin t && y <= ymax t

steps (x, y) (x', y') = let new = (x +x', y+y') in new : steps new (drag x', y'-1)

usefulsteps t vel = takeWhile (\(x,y) -> y > -77) $ steps (0,0) vel

hits t vel = any (insideTarget t) $ usefulsteps t vel

maxheight t vel = maximum $ map snd $ usefulsteps t vel

-- solutions
-- inp = parseInput "target area: x=287..309, y=-76..-48"
-- inp = parseInput "target area: x=20..30, y=-10..-5"
-- maximum $ map (maxheight inp) $ filter (hits inp) [(x,y) | x <- [0..50], y <- [0..200]]
-- length $ filter (hits inp) [(x,y) | x <- [0..310], y <- [-77..300]]
