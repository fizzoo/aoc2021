{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Lens
import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map as M
import Data.Maybe
-- import Debug.Trace

-- Nothing means that there was a flash during the current (intermediate) step
type B = M.Map (Int, Int) (Maybe Int)

width = 10

height = 10

twod = [(y, x) | y <- [0 .. width -1], x <- [0 .. width -1]]

inbounds (y, x) = y >= 0 && x >= 0 && y < width && x < width

readBoard :: [String] -> B
readBoard ls = M.fromList $ zip twod (map (Just . read . pure) $ concat ls)

surround :: (Int, Int) -> [(Int, Int)]
surround (y, x) = [(y + 1, x), (y + 1, x + 1), (y, x + 1), (y -1, x + 1), (y -1, x), (y -1, x -1), (y, x -1), (y + 1, x -1)]

showB :: B -> String
showB b = unlines $ replicate 10 '-' : [[f (b M.! (y,x)) | x <- [0..width-1]] | y <- [0..height-1]]
  where
    f (Just x) = head $ show x
    f Nothing = '?'

flash :: MonadState (B, Bool) m => m ()
flash = do
  let inc i = when (inbounds i) $ do
        c <- use (_1 . at i)
        case join c of
          Just x -> (_1 . ix i . _Just) %= (+ 1) >> _2 .= True
          _ -> return ()
      f i = do
        c <- use (_1 . at i)
        case join c of
          Just x -> when (x > 9) (mapM_ inc (surround i) >> _1 . ix i .= Nothing)
          _ -> return ()
      fUntilFix = do
        _2 .= False
        mapM_ f twod
        (b, c) <- get
        -- traceStack (showB b) (return ())
        changed <- use _2
        when changed fUntilFix

  _2 .= False
  mapM_ inc twod
  fUntilFix
  return ()

step :: B -> (B, Int)
step b = (fmap reset next, foldl (\a x -> if isNothing x then a+1 else a) 0 next)
  where
    next = fst $ execState flash (b, False)
    reset (Just x) = Just x
    reset Nothing = Just 0

stepN 0 acc b = (b, acc)
stepN n acc b = let (next, flashes) = step b in stepN (n-1) (acc+flashes) next

stepUntilAllFlash n b = if all (== Just 0) b then n else stepUntilAllFlash (n+1) (fst $ step b)

run f = do
  ls <- lines <$> readFile f
  let b = readBoard ls
  putStr $ showB b
  print $ stepN 100 0 b
  print $ stepUntilAllFlash 0 b
