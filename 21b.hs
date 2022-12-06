{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.MultiSet as MS

-- score p1, score p2, pos p1, pos p2. Had a Player Int Int structure
-- first but this allows sorting by both players score so taking the
-- min usually makes sense. Would be even better to sort by minimum of
-- score p1 and score p2 but w/e, not sure how to do that for the MS.
data Game = Game
  { _states :: MS.MultiSet (Int, Int, Int, Int, Bool),
    _p1wins, _p2wins :: Int
  }
  deriving (Show)

makeLenses ''Game

testgame = Game {_states = MS.fromList [(0, 0, 4, 8, False)], _p1wins = 0, _p2wins = 0}

realgame = Game {_states = MS.fromList [(0, 0, 9, 4, False)], _p1wins = 0, _p2wins = 0}

modp1 n m = mod (n -1) m + 1

step :: MonadState Game m => m ()
step = do
  st <- use states
  let cur = MS.findMin st
      num = MS.occur cur st
  states .= MS.deleteMinAll st
  let rolls = [x + y + z | x <- [1 .. 3], y <- [1 .. 3], z <- [1 .. 3]]
      isp2 = cur ^. _5
      pos_lens :: Lens' (Int, Int, Int, Int, Bool) Int
      pos_lens = if isp2 then _4 else _3
      score_lens :: Lens' (Int, Int, Int, Int, Bool) Int
      score_lens = if isp2 then _2 else _1
      cur_pos = cur ^. pos_lens
      new_states =
        [ cur & pos_lens .~ newpos
            & score_lens %~ (+ newpos)
            & _5 %~ not
          | c <- rolls,
            let newpos = modp1 (cur_pos + c) 10
        ]
      winning_states = filter (\st -> st ^. score_lens >= 21) new_states
      nonwinning_states = filter (\st -> st ^. score_lens < 21) new_states
  (if cur ^. _5 then p2wins else p1wins) %= (+ (length winning_states * num))
  states %= (`MS.union` MS.fromOccurList (zip nonwinning_states (repeat num)))
  return ()

stepOrDone :: MonadState Game m => m Bool
stepOrDone = do
  states_left <- length <$> use states
  p1w <- use p1wins
  p2w <- use p2wins
  if states_left == 0 then return True else step >> return False

dogame :: MonadState Game m => m ()
dogame = do
  done <- stepOrDone
  if done then return () else dogame

dog :: (MonadState Game m, MonadIO m) => m ()
dog = do
  done <- last <$> replicateM 100 stepOrDone
  states_left <- MS.distinctSize <$> use states
  liftIO $ print states_left
  if done then return () else dog

printStates :: Show a => [a] -> IO ()
printStates [] = return ()
printStates (s : xs) = do
  print s
  printStates xs
