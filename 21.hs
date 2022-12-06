{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.State.Strict

data Player = Player
  { _pos, _score :: Int
  }
  deriving (Show)

data Game = Game
  { _p1, _p2 :: Player,
    _nextDice, _numRolls :: Int
  }
  deriving (Show)

testgame = (Game {_p1 = Player 4 0, _p2 = Player 8 0, _nextDice=1, _numRolls=0})
realgame = (Game {_p1 = Player 9 0, _p2 = Player 4 0, _nextDice=1, _numRolls=0})

makeLenses ''Player
makeLenses ''Game

modp1 n m = mod (n -1) m + 1

getRoll :: MonadState Game m => m Int
getRoll = do
  d <- use nextDice
  nextDice %= (`modp1` 100) . (+ 1)
  numRolls %= (+ 1)
  return d

turn :: MonadState Game m => Lens' Game Player -> m ()
turn lp = do
  r1 <- getRoll
  r2 <- getRoll
  r3 <- getRoll
  pos <- lp . pos <%= (`modp1` 10) . (+ (r1 + r2 + r3))
  lp . score %= (+ pos)
  return ()

step :: MonadState Game m => m ()
step = do
  turn p1
  turn p2

dogame :: MonadState Game m => Bool -> m Int
dogame turnIsP2 = do
  if turnIsP2 then turn p2 else turn p1
  score1 <- use $ p1 . score
  score2 <- use $ p2 . score
  numrolls <- use numRolls
  if score1 >= 1000
    then return $ score2 * numrolls
    else
      if score2 >= 1000
        then return $ score1 * numrolls
        else dogame (not turnIsP2)
