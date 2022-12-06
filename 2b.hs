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

data SubState = Sub {x, y, aim :: Int}

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

runOneMove :: MonadState SubState m => Movement a -> m ()
runOneMove move = do
  Sub x y aim <- get
  case move of
    Forward i -> put $ Sub (x+i) (y+(i*aim)) aim
    Down i -> put $ Sub x y (aim+i)
    Up i -> put $ Sub x y (aim-i)

runMovement :: [Movement a] -> SubState
runMovement moves = execState (mapM runOneMove moves) (Sub 0 0 0)

runFile file = do
  input <- readFile file
  let Sub x y aim = runMovement $ map parseLine $ lines input
  print (x, y, aim, x*y)


main = runFile "2.in"
