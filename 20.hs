{-# LANGUAGE FlexibleContexts #-}

import Control.Exception
import Control.Lens
import qualified Data.Array as A

type Code = A.Array Int Bool
type Image = A.Array (Int,Int) Bool

twod h w = [(y, x) | y <- [0 .. h -1], x <- [0 .. w -1]]

padImage pad image = A.array ((yb',xb'),(ye',xe')) [((y,x), v) | y <- [yb'..ye'], x <- [xb'..xe'], let v = A.inRange bounds (y,x) && (image A.! (y,x))]
  where
    bounds@((yb, xb), (ye, xe)) = A.bounds image
    yb' = yb - pad
    xb' = xb - pad
    ye' = ye + pad
    xe' = xe + pad

readInput :: FilePath -> IO (A.Array Int Bool, A.Array (Int, Int) Bool)
readInput f = do
  ls <- lines <$> readFile f
  let code = A.array (0, 511) $ zip [0 .. 511] $ map (== '#') (head ls)
      imageStr = tail $ tail ls
      image = A.array ((0, 0), (length imageStr -1, length (head imageStr) - 1)) $ zip (twod (length imageStr) (length $ head imageStr)) $ map (== '#') $ concat imageStr
  return (code, image)

decimal' :: Int -> [Bool] -> Int
decimal' = foldl (\acc l -> 2 * acc + if l then 1 else 0)

decimal = decimal' 0

step :: Code -> Image -> Image
step code image = image & imapped %@~ g
  where
    bounds@(minbound,maxbound) = A.bounds image
    at loc = if A.inRange bounds loc then image A.! loc else image A.! minbound
    f (y, x) = decimal [at (y + yd, x + xd) | yd <- [-1 .. 1], xd <- [-1 .. 1]]
    g (y,x) v = code A.! f (y,x)

stepN 0 _ image = image
stepN n code image = stepN (n-1) code (step code image)

draw image = mapM_ putStrLn [[if image A.! (y, x) then '#' else '.' | x <- [xb .. xe]] | y <- [yb .. ye]]
  where
    ((yb, xb), (ye, xe)) = A.bounds image

countLit :: Image -> Integer
countLit = foldl (\acc x -> if x then acc+1 else acc) 0

-- countLit $ stepN 50 code $ padImage 51 image
