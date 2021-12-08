import Data.Bifunctor
import System.Exit
import System.IO

unique s = let n = length s in n == 2 || n == 3 || n == 4 || n == 7

main = do
  input <- map (second tail . break (=="|") . words) . lines <$> readFile "input"
  print $ length $ filter unique $ concatMap snd input
