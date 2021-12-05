import System.Exit
import System.IO

import Text.ParserCombinators.Parsec
import qualified Data.HashMap.Strict as HashMap

int = fmap (read :: String -> Int) (many1 digit)

parseLine = do
  x1 <- int
  char ','
  y1 <- int
  string " -> "
  x2 <- int
  char ','
  y2 <- int
  newline
  return ((x1,y1),(x2,y2))

parser = many parseLine

parseInput fileName = parseFromFile parser fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

linePoints acc ((x1,y1),(x2,y2))
  | x1 == x2 && y1 == y2 = (x1,y1) : acc
  | x1 == x2 && y1 > y2 = linePoints ((x1,y1) : acc) ((x1,y1-1),(x2,y2))
  | x1 == x2 && y1 < y2 = linePoints ((x1,y1) : acc) ((x1,y1+1),(x2,y2))
  | y1 == y2 && x1 > x2 = linePoints ((x1,y1) : acc) ((x1-1,y1),(x2,y2))
  | y1 == y2 && x1 < x2 = linePoints ((x1,y1) : acc) ((x1+1,y1),(x2,y2))
  | otherwise = undefined

countDup acc [] = acc
countDup acc (x : xs)
  | x `elem` xs = countDup (acc + 1) (filter (/=x) xs)
  | otherwise = countDup acc xs

onLine (x,y) ((x1,y1),(x2,y2))
  | x1 == x2 = x == x1 && ((y1 <= y && y <= y2) || (y2 <= y && y <= y1))
  | y1 == y2 = y == y1 && ((x1 <= x && x <= x2) || (x2 <= x && x <= x1))
  | otherwise = undefined

ones = 1 : ones

main = do
  lines <- filter (\((x1,y1),(x2,y2)) -> x1 == x2 || y1 == y2) <$> parseInput "input"
  print $ length $ filter (>=2) $ HashMap.elems $ HashMap.fromListWith (+) $ zip (foldl linePoints [] lines) ones
  -- print $ countDup 0 $ foldl linePoints [] lines
  -- print $ length $ filter (>=2) [length $ filter (onLine (i,j)) lines | i <- [0..999], j <- [0..999] ]
  return 0
