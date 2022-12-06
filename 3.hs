import Data.List (transpose)
import qualified Data.MultiSet

mostcommon :: Ord a => [a] -> a
mostcommon lst = snd $ maximum $ map (\(x, y) -> (y,x)) $ Data.MultiSet.toOccurList $ Data.MultiSet.fromList lst

bits :: [String] -> [Int]
bits lines = map (\x -> read [x] :: Int) commons
  where
  commons = map mostcommon $ transpose lines

invertBit :: Int -> Int
invertBit 0 = 1
invertBit 1 = 0
invertBit _ = undefined

bitsToInt x = f 1 (reverse x)
  where
    f i (x:xs) = i * x + f (i*2) xs
    f _ [] = 0

power :: [Int] -> Int
power lst = bitsToInt lst * bitsToInt (map invertBit lst)

runFile file = do
  input <- readFile file
  print $ power $ bits $ lines input


main = runFile "3test.in"
