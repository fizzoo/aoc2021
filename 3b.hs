import Data.List (transpose)
import qualified Data.MultiSet

getOccurList lst = map (\(x, y) -> (y,x)) $ Data.MultiSet.toOccurList $ Data.MultiSet.fromList lst

mostcommon :: Ord a => [a] -> a
mostcommon lst = snd $ maximum $ getOccurList lst

leastcommon :: Ord a => [a] -> a
leastcommon lst = snd $ minimum $ getOccurList lst

bits :: [String] -> [[Int]]
bits = map . map $ (\x -> read [x] :: Int)

findBits :: ([Int] -> Int) -> Int -> [[Int]] -> [Int]
findBits f idx bits = if length bits <= 1 then head bits else findBits f (idx+1) bits'
  where
  toKeep = f $ map (!! idx) bits
  bits' = filter (\x -> x !! idx == toKeep) bits

bitsToInt :: Num p => [p] -> p
bitsToInt x = f 1 (reverse x)
  where
    f i (x:xs) = i * x + f (i*2) xs
    f _ [] = 0

lifesupport :: [[Int]] -> Int
lifesupport bits = bitsToInt (findBits mostcommon 0 bits) * bitsToInt (findBits leastcommon 0 bits)

runFile file = do
  input <- readFile file
  print $ lifesupport $ bits $ lines input


main = runFile "3test.in"
