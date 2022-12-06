import Text.ParserCombinators.Parsec
import Data.Foldable

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
    f [a,b] = do
      char a
      nest <- many parseBT
      char b
      return $ BT [a,b] nest
    f _ = undefined

parseBTM = many parseBT

errorPoints line = case parse parseBTM "btm" line
  of Left error -> score $ sourceColumn $ errorPos error
     Right parse -> 0
  where
    score idx = if idx-1 >= length line then 0 else errorCharScore (line !! (idx-1))
    errorCharScore :: Char -> Int
    errorCharScore c
        | c == ')' = 3
        | c == ']' = 57
        | c == '}' = 1197
        | c == '>' = 25137
        | otherwise = error "no match"

runFile f = do
  ls <- lines <$> readFile f
  print $ map (parse parseBTM "btm") ls
  print $ map errorPoints ls
  print $ sum $ map errorPoints ls
