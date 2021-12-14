import Data.List

import System.Environment
import System.Exit
import System.IO

import Text.ParserCombinators.Parsec

parser = do
  template <- many alphaNum
  newline
  newline
  rules <- many parseLine
  return (zip template $ tail template, rules)
  where
    parseLine = do
      c1 <- anyChar
      c2 <- anyChar
      string " -> "
      c3 <- anyChar
      newline
      return ((c1,c2),((c1,c3),(c3,c2)))

parseInput fileName = parseFromFile parser fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

add m p [] = [(p,m)]
add m p ((q,n):t)
  | p == q = (q,n+m):t
  | otherwise = (q,n) : add m p t

step counts = foldl' (\acc (p,(n1,n2)) -> case p `lookup` counts of
                                            Nothing -> acc
                                            Just x -> add x n1 (add x n2 acc))
                     []

stepN 0 counts rules = counts
stepN n counts rules = stepN (n-1) (step counts rules) rules

countChars :: [((Char, Char), Int)] -> [Int]
countChars = map ((`div` 2) . snd) . foldr (\((c1,c2),n) acc -> add n c1 $ add n c2 acc) []

main = do
  args <- getArgs
  (template, rules) <- parseInput $ head args
  let counts = add 1 (last template) $ add 1 (head template) $ stepN 40 (map (\p -> (p,1)) template) rules
      nums = sort $ countChars counts
  print $ last nums - head nums
