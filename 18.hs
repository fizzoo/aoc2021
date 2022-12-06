{-# LANGUAGE FlexibleContexts #-}

import Data.Either
import Data.Maybe
import Debug.Trace
import Text.Parsec

type Parser = Parsec String ()

data Snailfish = SL Int | SP Snailfish Snailfish deriving (Show, Eq)

data LR = L | R deriving (Show)

type Path = [LR]

run p = runParser p () "lambda"

parseSL :: Parser Snailfish
parseSL = SL . read <$> many1 (choice $ map char ['0' .. '9'])

parseSP :: Parser Snailfish
parseSP = do
  char '['
  p1 <- parseSL <|> parseSP
  char ','
  p2 <- parseSL <|> parseSP
  char ']'
  return $ SP p1 p2

leftMost :: Snailfish -> Path
leftMost (SL _) = []
leftMost (SP s _) = L : leftMost s

rightMost :: Snailfish -> Path
rightMost (SL _) = []
rightMost (SP _ s) = R : rightMost s

goLeft :: Snailfish -> Path -> Maybe Path
goLeft (SL _) [] = Nothing
goLeft (SP s1 s2) (lr : rest) = case lr of
  L -> case goLeft s1 rest of
    Nothing -> Nothing
    Just p -> Just $ lr : p
  R -> case goLeft s2 rest of
    Nothing -> Just $ L : rightMost s1
    Just p -> Just $ lr : p
goLeft _ _ = undefined

goRight :: Snailfish -> Path -> Maybe Path
goRight (SL _) [] = Nothing
goRight (SP s1 s2) (lr : rest) = case lr of
  L -> case goRight s1 rest of
    Nothing -> Just $ R : leftMost s2
    Just p -> Just $ lr : p
  R -> case goRight s2 rest of
    Nothing -> Nothing
    Just p -> Just $ lr : p
goRight _ _ = undefined

access :: Snailfish -> Path -> Snailfish
access sf [] = sf
access (SP s _) (L : rest) = access s rest
access (SP _ s) (R : rest) = access s rest
access _ _ = undefined

modifyAt [] f sp = f sp
modifyAt (L : rest) f (SP s1 s2) = SP (modifyAt rest f s1) s2
modifyAt (R : rest) f (SP s1 s2) = SP s1 (modifyAt rest f s2)
modifyAt a _ c = traceShow (a, c) $ error "modifyAt"

modifyAtIf (Just p) f sp = modifyAt p f sp
modifyAtIf Nothing _ sp = sp

explodeAt :: Snailfish -> Path -> Snailfish
explodeAt sf p = modifyAtIf (goRight sf p >>= goRight sf) (\(SL n) -> SL $n + right) $ modifyAtIf (goLeft sf p) (\(SL n) -> SL $n + left) $ modifyAt (init p) (const $ SL 0) $ sf
  where
    (SL left) = access sf p
    (SL right) = access sf (fromMaybe (error "explodeAt p has no right") $ goRight sf p)

explodeP :: Snailfish -> Path -> Maybe Snailfish
explodeP sf p =
  if length p > 4
    then Just $ explodeAt sf p
    else case goRight sf p of
      Nothing -> Nothing
      Just p' -> explodeP sf p'

explode sf = explodeP sf (leftMost sf)

splitP sf p =
  if pred (access sf p)
    then Just $ modifyAt p (\(SL n) -> SP (SL $ floor $ fromIntegral n / 2) (SL $ ceiling $ fromIntegral n / 2)) sf
    else case goRight sf p of
      Nothing -> Nothing
      Just p' -> splitP sf p'
  where
    pred (SP _ _) = False
    pred (SL n) = n >= 10

split sf = splitP sf (leftMost sf)

reduce sf = case explode sf of
  Just sf' -> reduce sf'
  Nothing -> maybe sf reduce (split sf)

magnitude (SL n) = n
magnitude (SP s1 s2) = 3 * magnitude s1 + 2 * magnitude s2

runFile f = do
  ls <- lines <$> readFile f
  let sfs = fromRight (error "bad parse on input file") $ mapM (run parseSP) ls
      reduced = foldl1 (\a b -> reduce (SP a b)) sfs
  print reduced
  print $ magnitude reduced

runFile2 f = do
  ls <- lines <$> readFile f
  let sfs = fromRight (error "bad parse on input file") $ mapM (run parseSP) ls
  let magnitudes = [magnitude (reduce $ SP a b) | a <- sfs, b <- sfs, a /= b]
  print magnitudes
  print $ maximum magnitudes
