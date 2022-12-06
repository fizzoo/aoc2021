{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import Control.Lens
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.MultiSet as MS

type P3 = (Int, Int, Int)

data Scanner = Scanner
  { _name :: String,
    _beacons :: [P3],
    _distanceset :: MS.MultiSet Int,
    _location :: P3
  }
  deriving (Show)

makeLenses ''Scanner

parseInput :: String -> [Scanner]
parseInput inp = reverse $ foldl f [] $ filter (/= "") $ lines inp
  where
    uncomma = map (\c -> if c == ',' then ' ' else c)
    parseScanner s = Scanner (unwords $ init $ tail $ words s) [] MS.empty (-1234, 0, 1337)
    parseObs s = (\t -> (read $ t !! 0, read $ t !! 1, read $ t !! 2)) $ words $ uncomma s
    addbeac s o = Scanner (_name s) (_beacons s ++ [o]) MS.empty (_location s)
    f acc x = if take 3 x == "---" then parseScanner x : acc else addbeac (head acc) (parseObs x) : tail acc

rotx (x, y, z) = (x, - z, y)

roty (x, y, z) = (z, y, - x)

rotz (x, y, z) = (- y, x, z)

transformations = [a . b | a <- [id, roty, roty . roty, roty . roty . roty, rotz, rotz . rotz . rotz], b <- [id, rotx, rotx . rotx, rotx . rotx . rotx]]

manhattan :: P3 -> P3 -> Int
manhattan (x, y, z) (x', y', z') = abs (x - x') + abs (y - y') + abs (z - z')

asymcombos [] = []
asymcombos (x : xs) = map (x,) xs ++ asymcombos xs

addDistanceSet scanner = scanner & distanceset .~ set
  where
    set = MS.fromList $ map (uncurry manhattan) $ asymcombos $ scanner ^. beacons

distancesetIOU s1 s2 = fromIntegral (length intersec) / fromIntegral (length d1 + length d2 - length intersec)
  where
    d1 = s1 ^. distanceset
    d2 = s2 ^. distanceset
    intersec = MS.intersection d1 d2

p3minus :: P3 -> P3 -> P3
p3minus (x, y, z) (x', y', z') = (x - x', y - y', z - z')

tryTransform :: (P3 -> P3) -> Scanner -> Scanner -> Maybe (P3 -> P3, P3)
tryTransform trans s1 s2 = if top_offset_size >= 12 then Just (trans, top_offset) else Nothing
  where
    b1 = s1 ^. beacons
    b2 = s2 ^. beacons
    transformed_b2 = map trans b2
    offsets = MS.fromList [x2 `p3minus` x1 | x1 <- b1, x2 <- transformed_b2]
    (top_offset, top_offset_size) = maximumBy (\a b -> snd a `compare` snd b) $ MS.toOccurList offsets

findTransform :: Scanner -> Scanner -> Maybe (P3 -> P3, P3)
findTransform s1 s2 = asum [tryTransform trans s1 s2 | trans <- transformations]

findTransformedScanner :: Scanner -> Scanner -> Maybe Scanner
findTransformedScanner s1 s2 = f <$> findTransform s1 s2
  where
    f (fn, offset) =
      s2 & beacons . mapped %~ (\x -> fn x `p3minus` offset)
        & location .~ offset

joinMaps known [] = return known
joinMaps known unknown = do
  let matching = sortBy (\a b -> compare (b ^. _1) (a ^. _1)) $ [(distancesetIOU a b, a, b) | a <- known, b <- unknown]
  putStr "num known,unknown: "
  print (length known, length unknown)
  putStr "top matches: "
  print $ take 3 $ matching & traverse . _2 %~ _name & traverse . _3 %~ _name
  let m = fromMaybe (error "No match at all in joinMaps") $ asum $ map (\(x, y, z) -> findTransformedScanner y z) matching
  joinMaps (known ++ [m]) (deleteBy (\a b -> a ^. name == b ^. name) m unknown)

run inp = do
  let scanners = parseInput inp
      distanced = map addDistanceSet scanners
  transformed_scanners <- joinMaps [head distanced & location .~ (0, 0, 0)] (tail distanced)
  let allbeacons = MS.fromList $ concatMap _beacons transformed_scanners
  putStr "number of beacons: "
  print $ MS.distinctSize allbeacons
  print $ maximum $ map (uncurry manhattan) $ asymcombos $ map _location transformed_scanners
  return ()
