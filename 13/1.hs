import Data.List

import System.Exit
import System.IO

import Text.ParserCombinators.Parsec

data Fold = Horiz Int | Vert Int
  deriving (Show)

makeFold 'x' n = Vert n
makeFold 'y' n = Horiz n
makeFold _ _ = undefined

parser = do
  points <- many parsePoint
  newline
  folds <- many parseFold
  return (points, folds)
  where
    parsePoint = do
      x <- readInt <$> many digit
      char ','
      y <- readInt <$> many digit
      newline
      return (x, y)
    parseFold = do
      string "fold along "
      dir <- anyChar
      char '='
      amount <- readInt <$> many digit
      newline
      return $ makeFold dir amount
    readInt = read :: (String -> Int)

parseInput fileName = parseFromFile parser fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

add x lst
  | x `elem` lst = lst
  | otherwise = x : lst

doFold _ [] = []
doFold (Horiz n) ((x,y):t)
  | y > n = add (x,n-(y-n)) $ doFold (Horiz n) t
  | otherwise = add (x,y) $ doFold (Horiz n) t
doFold (Vert n) ((x,y):t)
  | x > n = add (n-(x-n),y) $ doFold (Vert n) t
  | otherwise = add (x,y) $ doFold (Vert n) t

main = do
  (points, folds) <- parseInput "input"
  print $ length $ doFold (head folds) points

