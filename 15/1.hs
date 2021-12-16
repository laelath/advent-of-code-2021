import Data.Array
import Data.Char
import Data.Maybe

import System.Environment

type Grid = Array (Int, Int) Int
type Queue = [(Int, [(Int, Int)])]

parseGrid :: [[Char]] -> Grid
parseGrid inLines = listArray ((1,1), (m,n)) (map digitToInt (concat inLines))
  where
    m = length inLines
    n = length (head inLines)

enqueue :: (Int, (Int, Int)) -> Queue -> Queue
enqueue (d, p) [] = [(d, [p])]
enqueue (d1, p1) ((d2, lp) : t)
  | d1 < d2 = (d1, [p1]) : (d2, lp) : t
  | d1 == d2 =  (d2, p1 : lp) : t
  | otherwise = (d2, lp) : enqueue (d1, p1) t

dequeue :: Queue -> Maybe ((Int, (Int, Int)), Queue)
dequeue [] = Nothing
dequeue ((_, []) : rst) = dequeue rst
dequeue ((d, h : t) : rst) = Just ((d, h), (d, t) : rst)

search :: Grid -> (Int,Int) -> [(Int,Int)] -> Queue -> Int
search grid end seen queue
  | p == end = dist
  | p `elem` seen = search grid end seen queue'
  | otherwise = search grid end (p : seen) $ foldr enqueue queue' (neighbors p)
  where
    ((dist, p), queue') = fromJust $ dequeue queue
    neighbors :: (Int, Int) -> [(Int, (Int,Int))]
    neighbors (i, j) = [ (dist + grid ! (i', j'), (i', j'))
                       | (i', j') <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
                       , i' >= 1 && i' <= m && j' >= 1 && j' <= n
                       ]
    (_, (m, n)) = bounds grid

main = do
  args <- getArgs
  grid <- parseGrid . lines <$> readFile (head args)
  print $ search grid (snd $ bounds grid) [] [(0,[(1,1)])]
