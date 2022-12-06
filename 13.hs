{-# LANGUAGE FlexibleContexts #-}

import Control.Lens hiding (Fold)
import Data.List
import qualified Data.Map as M
import Data.Maybe

type Paper = [(Int, Int)]

data Fold = AlongY Int | AlongX Int deriving (Show)

makeFold "x" n = AlongX n
makeFold "y" n = AlongY n
makeFold _ _ = error "incomplete makeFold"

getInput f = do
  ls <- lines <$> readFile f
  let (dotsI, foldsI) = break null ls & _2 %~ tail
      parseTuple x = read ("(" ++ x ++ ")") :: (Int, Int)
      parseFold x = uncurry makeFold $ (_2 %~ read . tail) $ break (== '=') $ fromMaybe (error "failed strip") $ stripPrefix "fold along " x

  return (sort $ map parseTuple dotsI, map parseFold foldsI)

fold :: Fold -> Paper -> Paper
fold f p = nub . sort $ map fn p
  where
    fn x = case f of
      AlongX n -> if x ^. _1 > n then x & _1 %~ (2 * n -) else x
      AlongY n -> if x ^. _2 > n then x & _2 %~ (2 * n -) else x

foldall :: [Fold] -> Paper -> Paper
foldall fs p = foldl (flip fold) p fs

draw :: Paper -> IO ()
draw p = do
  let mx = maximum $ p ^.. traverse . _1
      my = maximum $ p ^.. traverse . _2
      ls = [[if (x, y) `elem` p then '#' else '.' | x <- [0 .. mx]] | y <- [0 .. my]]
  putStrLn $ unlines ls

run f = do
  (p,f) <- getInput f
  draw $ foldall f p
