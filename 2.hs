{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Lens
import Control.Monad.State.Lazy

data Movement a where
  Forward :: Int -> Movement Int
  Down :: Int -> Movement Int
  Up :: Int -> Movement Int

deriving instance Show a => Show (Movement a)

parseLine :: String -> Movement Int
parseLine s = constructor (read var :: Int)
  where
  constructor = if | command == "forward" -> Forward
                   | command == "down" -> Down
                   | command == "up" -> Up
                   | otherwise -> undefined
  word = words s
  command = head word
  var = word !! 1

runOneMove :: MonadState (Int, Int) m => Movement a -> m ()
runOneMove move = do
  (x, y) <- get
  case move of
    Forward i -> put (x+i, y)
    Down i -> put (x, y+i)
    Up i -> put (x, y-i)

runMovement :: [Movement a] -> (Int, Int)
runMovement moves = execState (mapM runOneMove moves) (0, 0)

main = do
  input <- readFile "2.in"
  let (x, y) = runMovement $ map parseLine $ lines input
  print $ (x, y, x*y)
