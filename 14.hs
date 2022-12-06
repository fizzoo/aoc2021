import Control.Lens
import Data.List

getInput f = do
  ls <- lines <$> readFile f
  let template = head ls
      mappings = tail $ tail ls
      pairs = map (\x -> (take 2 x, x !! 6)) mappings

  return (template, pairs)

geta = getInput "14a.in"

getb = getInput "14b.in"

step pairs template = mapped
  where
    zipped = zip template (tail template)
    f (a, b) = case lookup [a, b] pairs of
      Just x -> [a, x, b]
      Nothing -> [a, b]
    joinExcept1 ls = (ls >>= init) ++ [last $ last ls]
    mapped = joinExcept1 $ map f zipped

stepN 0 _ template = template
stepN n pairs template = stepN (n -1) pairs (step pairs template)

score ls = (minimum m, maximum m, fst (maximum m) - fst (minimum m))
  where
    m = map (\ls -> (length ls, head ls)) $ group $ sort ls
