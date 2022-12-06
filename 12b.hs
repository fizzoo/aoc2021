{-# LANGUAGE FlexibleContexts #-}

import Control.Lens

readInput f = do
  ls <- lines <$> readFile f
  let paths = map ((_2 %~ tail) . break (== '-') ) ls
      symmetric = paths ++ map (\(x,y) -> (y,x)) paths
  return symmetric

geta = readInput "12a.in"

getb = readInput "12b.in"

getc = readInput "12c.in"

getd = readInput "12d.in"

availableMoves :: Eq a => [(a, b)] -> a -> [b]
availableMoves lst a = map snd $ filter ((== a) . fst) lst

capitalized c = c `elem` ['A'..'Z']

-- paths are reversed for quick access to the last elem
paths :: [(String, String)] -> Bool -> [String] -> [[String]]
paths _ _ [] = error "need something in path to start"
paths lst did_twice path@(lastcave:_) = if lastcave == "end" then [path] else n'
  where
    ok :: String -> Bool
    ok cave = cave /= "start" && (not did_twice || any capitalized cave || notElem cave path)
    n = filter ok $ availableMoves lst lastcave
    n_w_revisit = map (\cave -> (cave, not (any capitalized cave) && cave `elem` path)) n
    n' = n_w_revisit >>= \(new, revisit) -> paths lst (did_twice || revisit) (new:path)
