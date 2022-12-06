{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Lens
import Data.List
import qualified Data.Map as M
import Data.Foldable

lengthToDigit :: M.Map Integer [Integer]
lengthToDigit = M.fromList [(2, [1]), (3, [7]), (4, [4]), (5, [2, 3, 5]), (6, [0, 6, 9]), (7, [8])]

digitToSection :: M.Map Integer String
digitToSection = M.fromList [(0, "ABCEFG"), (1, "CF"), (2, "ACDEG"), (3, "ACDFG"), (4, "BCDF"), (5, "ABDFG"), (6, "ABDEFG"), (7, "ACF"), (8, "ABCDEFG"), (9, "ABCDFG")]

sectionToDigit = M.fromList $ map (\(a,b) -> (b,a)) $ M.toList digitToSection

lengthToSection :: Integer -> [String]
lengthToSection x = (digitToSection M.!) <$> (lengthToDigit M.! x)

getFileInput file = do
  input <- readFile file
  return $ traverse . _2 %~ tail $ map (break (== "|") . words) . lines $ input

data ConstraintState = CS
  { _can_be :: M.Map Char String,
    _remaining :: String
  }
  deriving (Show)

makeLenses ''ConstraintState

emptyCS :: ConstraintState
emptyCS = CS M.empty "abcdefg"

generatePossibilities :: String -> String -> M.Map Char String
generatePossibilities x y = M.fromList $ map (,y) x

addPossibilityToCS p cs = cs & can_be %~ M.unionWith intersect p

generateCS :: [String] -> ConstraintState -> [ConstraintState]
generateCS [] cs = [cs]
generateCS (l : ls) cs = map (`addPossibilityToCS` cs) possibilities >>= generateCS ls
  where
    possibilities = generatePossibilities l <$> lengthToSection (fromIntegral $ length l)

initialCS :: [String] -> [ConstraintState]
initialCS ls = generateCS ls emptyCS

try1 :: ConstraintState -> Either ConstraintState [ConstraintState]
try1 cs = if | any null m -> Right []
             | null $ cs ^. remaining -> Left cs
             | otherwise -> Right $ map (next cs c) p
  where
    m = cs ^.. can_be.traversed
    (c:rem) = cs ^. remaining
    p = (cs ^. can_be) M.! c
    next cs k v =
      cs & can_be . imapped %@~ (\i c -> if i == k then [v] else filter (/= v) c)
        & remaining .~ rem

try :: ConstraintState -> Maybe ConstraintState
try cs = case try1 cs of
  Left win -> Just win
  Right [] -> Nothing
  Right css -> asum $ map try css

solveConstraint :: [String] -> Maybe ConstraintState
solveConstraint = asum . map try . initialCS

addDigits :: [Integer] -> Integer
addDigits ls = f 1 $ reverse ls
  where
  f n (x:xs) = x * n + f (10*n) xs
  f _ [] = 0

-- lhs, rhs
solve :: [String] -> [String] -> Integer
solve examples digits = addDigits $ map f digits
  where
    m = solveConstraint examples ^. _Just.can_be
    f :: String -> Integer
    f x = sectionToDigit M.! (sort . map (\c -> head $ m M.! c) $ x)

runFile file = do
  inputs <- getFileInput file
  -- print $ sum $ map (length . filter (\x -> length x `elem` [2, 3, 4, 7])) $ inputs ^.. traverse . _2
  let outs = uncurry solve <$> inputs
  print outs
  print $ sum outs
