{-# LANGUAGE FlexibleContexts #-}

import Control.Exception
import Control.Lens
import Control.Monad.State
import qualified Data.Array as A
import Data.Array.Lens
import Data.Heap
import Data.Maybe
import Debug.Trace

readInput :: FilePath -> IO (A.Array (Int, Int) Int)
readInput f = do
  ls <- lines <$> readFile f
  let w = length $ head ls
      h = assert (length ls == w) w
  putStr "input size is: "
  print (w, h)
  let arr = A.array ((0, 0), (w -1, w -1)) [((y, x), read [v] :: Int) | (y, l) <- zip [0 ..] ls, (x, v) <- zip [0 ..] l]
  return arr

plz = fromMaybe (error "bad index")

mincost :: MonadState (A.Array (Int, Int) Int) m => A.Array (Int, Int) Int -> MinPrioHeap Int (Int, Int) -> (Int, Int) -> m Int
mincost arr heap (ty, tx) = do
  let ((cost, loc@(ny, nx)), nheap) = plz $ Data.Heap.view heap
      ok loc = A.inRange (A.bounds arr) loc
      insertIfOk loc heap = if ok loc then insert (cost + plz (arr ^? ix loc), loc) heap else heap
  prev_cost <- plz <$> preuse (ix loc)
  if loc == (ty, tx)
    then return cost
    else
      if cost < prev_cost
        then ix loc .= cost >> mincost arr (nheap & insertIfOk (ny -1, nx) & insertIfOk (ny + 1, nx) & insertIfOk (ny, nx -1) & insertIfOk (ny, nx + 1)) (ty, tx)
        else mincost arr nheap (ty, tx)

expand a =
  A.array
    ((0, 0), (neww -1, neww -1))
    [ ((y, x), v')
      | y <- [0 .. neww -1],
        let (ydiv, ymod) = y `divMod` w,
        x <- [0 .. neww -1],
        let (xdiv, xmod) = x `divMod` w,
        let v = plz (a ^? ix (ymod, xmod)) + ydiv + xdiv,
        let v' = if v >= 10 then v -9 else v
    ]
  where
    w = A.bounds a ^. _2 . _1 + 1
    neww = w * 5

run f = do
  inp <- readInput f
  let ninp = expand inp
  let (score, board) = runState (mincost ninp (fromList [(0, (0, 0))]) (snd $ A.bounds ninp)) (99999 <$ ninp)
  print board
  print score

-- This impl ended up veeery slow for some reason at 381s. Changing so to only adding in insertIfOk when new_cost < old_cost while modifying in the state did not help.
main = run "15b.in"
