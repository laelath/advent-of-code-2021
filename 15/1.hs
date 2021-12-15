import Data.Char
import Data.List

import qualified Data.HashMap as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.PSQueue as Queue

import System.Environment
import System.IO

coords :: Int -> [[a]] -> [(Int,Int)]
coords _ [] = []
coords n (h:t) = [(n,m) | m <- [0..length h - 1]] ++ coords (n+1) t

lookupPoint (0,0) ((v:tx):ty) = Just v
lookupPoint (x,0) ((_:tx):ty) = lookupPoint (x-1,0) (tx:ty)
lookupPoint (x,y) (_:ty) = lookupPoint (x,y-1) ty
lookupPoint _ _ = Nothing

-- lookupPoint (x,y) grid
--   | x < 0 || y < 0 = Nothing
--   | y >= length grid = Nothing
--   | x >= length (grid !! y) = Nothing
--   | otherwise = Just $ (grid !! y) !! x

manhattan (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

-- gridBFS :: [[Int]] -> (Int,Int) -> [(Int,(Int,Int))] -> Int
gridBFS grid end seen ((w,p):t)
  | p == end = w
  | otherwise = gridBFS grid end (HashSet.insert p seen) $ insertAdjs w p t
  where
    insertAdjs :: Int -> (Int,Int) -> [(Int,(Int,Int))] -> [(Int,(Int,Int))]
    insertAdjs w p l = foldl' (\acc q -> case HashMap.lookup q grid of
                                           Nothing -> acc
                                           Just v -> insert (w + v + manhattan q end - manhattan p end, q) acc)
                              l (adjPoints p)
    adjPoints (x,y) = filter (not . (`HashSet.member` seen)) [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]
gridBFS _ _ _ [] = undefined

main = do
  args <- getArgs
  grid <- map (map digitToInt) . lines <$> readFile (head args)
  let end = (length (last grid) - 1,length grid - 1)
  print $ gridBFS (HashMap.fromList (zip (coords 0 grid) (concat grid))) end HashSet.empty [(manhattan (0,0) end,(0,0))]
