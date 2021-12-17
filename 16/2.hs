import Data.List
import System.Environment

import Text.Parsec

decodeHex :: Char -> [Bool]
decodeHex '0' = [ False, False, False, False ]
decodeHex '1' = [ False, False, False, True  ]
decodeHex '2' = [ False, False, True,  False ]
decodeHex '3' = [ False, False, True,  True  ]
decodeHex '4' = [ False, True,  False, False ]
decodeHex '5' = [ False, True,  False, True  ]
decodeHex '6' = [ False, True,  True,  False ]
decodeHex '7' = [ False, True,  True,  True  ]
decodeHex '8' = [ True,  False, False, False ]
decodeHex '9' = [ True,  False, False, True  ]
decodeHex 'A' = [ True,  False, True,  False ]
decodeHex 'B' = [ True,  False, True,  True  ]
decodeHex 'C' = [ True,  True,  False, False ]
decodeHex 'D' = [ True,  True,  False, True  ]
decodeHex 'E' = [ True,  True,  True,  False ]
decodeHex 'F' = [ True,  True,  True,  True  ]
decodeHex _ = undefined

binToInt :: [Bool] -> Int
binToInt = foldl' (\acc b -> if b then 2 * acc + 1 else 2 * acc) 0

parseBin :: Int -> Parsec [Bool] () Int
parseBin n = binToInt <$> count n anyToken

parseLiteral :: Parsec [Bool] () Int
parseLiteral = parseLiteral' 0
  where
    parseLiteral' :: Int -> Parsec [Bool] () Int
    parseLiteral' acc = do
      more <- anyToken
      num <- parseBin 4
      if more then
        parseLiteral' (acc * 16 + num)
      else
        return (acc * 16 + num)

parsePacket :: Parsec [Bool] () Int
parsePacket = do
  version <- parseBin 3
  typeID <- parseBin 3
  if typeID == 4 then
    parseLiteral
  else do
    i <- anyToken
    if i then do
      nPackets <- parseBin 11
      subPackets <- count nPackets parsePacket
      return $ doOp typeID subPackets
    else do
      nBits <- parseBin 15
      bits <- count nBits anyToken
      case parse (many1 parsePacket) "" bits of
        Left e -> fail $ show e
        Right packets -> return $ doOp typeID packets

cmp :: (Int -> Int -> Bool) -> [Int] -> Int
cmp f [v1, v2] = if f v1 v2 then 1 else 0
cmp _ _ = undefined

doOp :: Int -> [Int] -> Int
doOp 0 = sum
doOp 1 = product
doOp 2 = minimum
doOp 3 = maximum
doOp 5 = cmp (>)
doOp 6 = cmp (<)
doOp 7 = cmp (==)
doOp _ = undefined

main = do
  args <- getArgs
  bits <- concatMap decodeHex . concat . lines <$> readFile (head args)
  print $ parse parsePacket (head args) bits
