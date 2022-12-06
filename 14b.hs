{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
import Control.Lens
import Data.List
import Data.Maybe

groupByFst :: (Ord a, Ord b) => [(a, b)] -> [(a, [b])]
groupByFst = map (\ls -> (fst $ head ls, map snd ls)) . groupBy (\x y -> fst x == fst y) . sort

groupBySums :: (Ord a, Ord b, Num b) => [(a, b)] -> [(a, b)]
groupBySums ls = groupByFst ls & traverse._2 %~ sum

getInput f = do
  ls <- lines <$> readFile f
  let template = head ls
      mappinglines = tail $ tail ls
      pairs = groupBySums $ zipWith (\a b -> ([a, b], 1)) template (tail template)
      mapping = map (\x -> (take 2 x, x !! 6)) mappinglines

  return (mapping, pairs, [last template])

geta :: IO ([([Char], Char)], [([Char], Integer)], [Char])
geta = getInput "14a.in"

getb :: IO ([([Char], Char)], [([Char], Integer)], [Char])
getb = getInput "14b.in"

step mapping pairs = mapped
  where
    f (from@[a, b], cnt) = case lookup from mapping of
      Just new -> [([a, new], cnt), ([new, b], cnt)]
      Nothing -> [([a, b], cnt)]
    f _ = error "no match"
    mapped = groupBySums $ pairs >>= f

stepN 0 _ p = p
stepN n m p = stepN (n -1) m (step m p)

counts pairs ends = groupBySums $ (traverse._1 %~ head $ pairs) ++ map (,1) ends

run f = do
  (m,p,e) <- getInput f
  let cnts = counts (stepN 40 m p) e
  print cnts
  let nums = map snd cnts
  print (maximum nums,minimum nums)
  print (maximum nums - minimum nums)

-- score ls = (minimum m, maximum m, fst (maximum m) - fst (minimum m))
