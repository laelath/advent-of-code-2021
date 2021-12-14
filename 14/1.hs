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
  return (template, rules)
  where
    parseLine = do
      c1 <- anyChar
      c2 <- anyChar
      string " -> "
      c3 <- anyChar
      newline
      return ((c1,c2),c3)

parseInput fileName = parseFromFile parser fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

applyRules rules template =
  reverse $ last template :
    foldl' (\acc (c1,c2) -> case (c1,c2) `lookup` rules of
                              Nothing -> c1 : acc
                              Just c -> c : c1 : acc)
           [] (zip template $ tail template)

applyNTimes 0 rules template = template
applyNTimes n rules template = applyNTimes (n-1) rules (applyRules rules template)

main = do
  args <- getArgs
  (template, rules) <- parseInput $ head args
  let gs = sort $ map length $ group $ sort (applyNTimes 10 rules template)
  print $ last gs - head gs
