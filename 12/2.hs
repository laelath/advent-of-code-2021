import Data.Char
import Data.List
import Data.Tuple

import System.Exit
import System.IO

import Text.ParserCombinators.Parsec

data Cave = Start | End | Large String | Small String
  deriving (Eq, Show)

readCave s
  | s == "start" = Start
  | s == "end" = End
  | all isUpper s = Large s
  | otherwise = Small s

parser = many parseLine
  where
    parseLine = do
      s <- many alphaNum
      char '-'
      e <- many alphaNum
      newline
      return (readCave s, readCave e)

parseInput fileName = parseFromFile parser fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

traverseFrom c acc [] = acc
traverseFrom c acc ((s,e):t)
  | c == s = traverseFrom c (e : acc) t
  | c == e = traverseFrom c (s : acc) t
  | otherwise = traverseFrom c acc t

validPath _ _ [] = True
validPath seen two ((Small s):t)
  | s `elem` seen = not two && validPath seen True t
  | otherwise = validPath (s:seen) two t
validPath seen two (h:t) = validPath seen two t

canVisit _ Start = False
canVisit l (Small s) = validPath [] False ((Small s):l)
canVisit _ _ = True

findPaths graph acc [] = acc
findPaths graph acc (h:t) =
  findPaths graph (map reverse completed ++ acc) (nexts ++ t)
  where
    (completed, nexts) = partition ((==End) . head) $ map (:h) $ filter (canVisit h) $ traverseFrom (head h) [] graph

main = do
  graph <- parseInput "input"
  print $ length $ findPaths graph [] [[Start]]
