{-# LANGUAGE FlexibleContexts #-}

import Data.Either
import Debug.Trace
import Text.Parsec

type Parser = Parsec String Int

run p = runParser p 0 "lambda"

hexToBits '0' = "0000"
hexToBits '1' = "0001"
hexToBits '2' = "0010"
hexToBits '3' = "0011"
hexToBits '4' = "0100"
hexToBits '5' = "0101"
hexToBits '6' = "0110"
hexToBits '7' = "0111"
hexToBits '8' = "1000"
hexToBits '9' = "1001"
hexToBits 'A' = "1010"
hexToBits 'B' = "1011"
hexToBits 'C' = "1100"
hexToBits 'D' = "1101"
hexToBits 'E' = "1110"
hexToBits 'F' = "1111"
hexToBits _ = error "invalid hex"

getBits ls = ls >>= hexToBits

decimal :: String -> Int
decimal = foldl (\acc x -> 2 * acc + read [x]) 0

data Packet = Literal Int Int | Op Int Int [Packet] deriving (Show)

b0 = do
  char '0'
  modifyState succ

  return '0'

b1 = do
  char '1'
  modifyState succ
  return '1'

bit = b0 <|> b1

literalPacket :: Parser Packet
literalPacket = do
  version <- decimal <$> count 3 bit
  sequence_ [b1, b0, b0]
  nums <- many $ b1 >> count 4 bit
  last_num <- b0 >> count 4 bit
  return $ Literal version $ decimal $ concat nums ++ last_num

subPacketsByNumPackets :: Int -> Parser [Packet]
subPacketsByNumPackets n = count n packet

subPacketsByLength :: Int -> Parser [Packet]
subPacketsByLength n
  | n < 0 = error "length mismatch"
  | n == 0 = return []
  | otherwise = do
    lenBefore <- getState
    p <- packet
    lenAfter <- getState
    (p :) <$> subPacketsByLength (n - (lenAfter - lenBefore))

operatorPacket :: Parser Packet
operatorPacket = do
  version <- decimal <$> count 3 bit
  op <- decimal <$> count 3 bit
  lengthBit <- bit
  len <- decimal <$> if lengthBit == '0' then count 15 bit else count 11 bit
  subpackets <- if lengthBit == '1' then subPacketsByNumPackets len else subPacketsByLength len
  return $ Op version op subpackets

packet :: Parser Packet
packet = try literalPacket <|> operatorPacket

versionSum (Op v _ packets) = v + sum (map versionSum packets)
versionSum (Literal v _) = v

compute (Op _ 0 packets) = sum $ map compute packets
compute (Op _ 1 packets) = product $ map compute packets
compute (Op _ 2 packets) = minimum $ map compute packets
compute (Op _ 3 packets) = maximum $ map compute packets
compute (Op _ 5 packets) = let ls = map compute packets in if head ls > ls !! 1 then 1 else 0
compute (Op _ 6 packets) = let ls = map compute packets in if head ls < ls !! 1 then 1 else 0
compute (Op _ 7 packets) = let ls = map compute packets in if head ls == ls !! 1 then 1 else 0
compute (Literal _ lit) = lit
compute _ = error "bad compute: Invalid Packet"
