import Text.ParserCombinators.Parsec
import System.Exit
import System.IO
import Data.List
import Data.Maybe

type Board = [[Maybe Integer]]

integer = fmap (read :: String -> Integer) (many1 digit)

sepByCount n p s = do
  a <- p
  b <- count (n-1) skipSThenP
  return (a : b)
  where
    skipSThenP = do
      s
      p

parseBoard = count 5 parseBoardLine
  where
    parseBoardLine = do
      optional space
      nums <- sepByCount 5 (fmap Just integer) spaces
      newline
      return nums

parser = do
  nums <- integer `sepBy` char ','
  count 2 newline
  boards <- parseBoard `sepBy` newline
  eof
  return (nums,boards)

parseInput fileName = parseFromFile parser fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

mark n = map $ map (\m -> if m == Just n then Nothing else m)

hasBingo b = any (all (==Nothing)) b || any (all (==Nothing)) (transpose b)

findWinning (n:nums) boards =
  let marked = map (mark n) boards
      bingos = filter hasBingo marked
  in
  if null bingos then
    findWinning nums marked
  else
    (n,head bingos)
findWinning _ _ = undefined

sumBoard = sum . map (fromMaybe 0) . concat

-- splitOn c s =
--   case break (==c) s of
--     (a, ',':b) -> a : splitOn c b
--     (a, "")    -> [a]
--     _          -> undefined

main = do
  -- input <- fmap lines $ readFile "input"
  -- let numbers = map read $ splitOn ',' $ head input :: [Integer]
  --     boards = readBoards $ tail input
  (nums,boards) <- parseInput "input"
  let (n,board) = findWinning nums boards
  print (n * sumBoard board)
  return 0
