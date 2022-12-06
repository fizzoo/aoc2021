{-# LANGUAGE FlexibleContexts #-}

import Control.Lens
import Data.Array (array, (!))
import qualified Data.Array as A
import Debug.Trace

t1 = "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>"

t2 = "...>>>>>...\n...>>>>>..."

t3 = "..........\n.>v....v..\n.......>..\n.........."

data Cucumber = E | S deriving (Eq, Show)

type Space = Maybe Cucumber

type Board = A.Array (Int, Int) (Maybe Cucumber)

parseSpace :: Char -> Maybe Cucumber
parseSpace '.' = Nothing
parseSpace '>' = Just E
parseSpace 'v' = Just S
parseSpace _ = undefined

parseInput :: String -> Board
parseInput s = array ((0, 0), (h -1, w -1)) (zip [(y, x) | y <- [0 .. h -1], x <- [0 .. w -1]] (map parseSpace $ concat ls))
  where
    ls = lines s
    w = length $ head ls
    h = length ls

readInput f = parseInput <$> readFile f

drawBoard :: Board -> IO ()
drawBoard b = mapM_ putStrLn [[f (b ! (y, x)) | x <- [xs .. xe]] | y <- [ys .. ye]]
  where
    ((ys, xs), (ye, xe)) = A.bounds b
    f Nothing = '.'
    f (Just x) = case x of
      E -> '>'
      S -> 'v'

stepE :: Board -> Board
stepE b = b & imapped %@~ f
  where
    (_, (ye, xe)) = A.bounds b
    at a (y, x) = a ! (mod y (ye + 1), mod x (xe + 1))
    f (y, x) Nothing = if b `at` (y, x -1) == Just E then Just E else Nothing
    f (y, x) (Just E) = if b `at` (y, x + 1) == Nothing then Nothing else Just E
    f (y, x) (Just S) = Just S

stepS :: Board -> Board
stepS b = b & imapped %@~ f
  where
    (_, (ye, xe)) = A.bounds b
    at a (y, x) = a ! (mod y (ye + 1), mod x (xe + 1))
    f (y, x) Nothing = if b `at` (y -1, x) == Just S then Just S else Nothing
    f (y, x) (Just E) = Just E
    f (y, x) (Just S) = if b `at` (y + 1, x) == Nothing then Nothing else Just S

step = stepS . stepE

stepN 0 b = b
stepN n b = stepN (n -1) (stepS $ stepE b)

stepUntilFixed n b = let b' = step b in if b' == b then (n, b) else stepUntilFixed (n + 1) b'
