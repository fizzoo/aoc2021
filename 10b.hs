{-# LANGUAGE StandaloneDeriving #-}

import Data.Foldable
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.Either

deriving instance Show Message

data BracketTree = BT String [BracketTree] deriving (Show)

okSingletons = ["()", "[]", "{}", "<>"]

singletonBT s = if s `elem` okSingletons then BT s [] else error "failed parse"

parseSingle :: Parser BracketTree
parseSingle = singletonBT <$> (f1 <|> f2 <|> f3 <|> f4)
  where
    f1 = string "()"
    f2 = string "[]"
    f3 = string "{}"
    f4 = string "<>"

parseMultiple = many parseSingle

parseBT :: Parser BracketTree
parseBT = asum $ map f okSingletons
  where
    f [a, b] = do
      char a
      nest <- many parseBT
      char b
      return $ BT [a, b] nest
    f _ = undefined

parseBTM = many parseBT

getIfExpected err = case err of
  Expect s -> [s !! 1]
  _ -> ""

-- Get SysUnExpect "" in some cases, that one doesn't count.
isUnexpect err = case err of
  UnExpect _ -> True
  SysUnExpect [] -> False
  SysUnExpect _ -> True
  _ -> False

errorCompletion errs = if any isUnexpect errs then "" else last . filter (not . null) $ map getIfExpected errs

completion1 line = case parse parseBTM "btm" line of
  Left error -> errorCompletion $ errorMessages error
  Right parse -> ""

completion line = let c = completion1 line in if c == "" then "" else c ++ completion (line ++ c)

score str = foldl (\a x -> a * 5 + x) 0 $ map sc str
  where
    sc x
      | x == ')' = 1
      | x == ']' = 2
      | x == '}' = 3
      | x == '>' = 4
      | otherwise = error "case"

runFile f = do
  ls <- lines <$> readFile f
  print $ map (parse parseBTM "btm") ls
  print $ map completion ls
  print $ map (score . completion) ls
  let lst = filter (/= 0) $ sort $ map (score . completion) ls
  print (lst !! ((length lst `div` 2)))
