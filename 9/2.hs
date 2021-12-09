import Data.Char
import Data.List
import Data.Maybe

nexts [] = []
nexts (x:xs) = take 1 xs : nexts xs
prevs = reverse . nexts . reverse

adjacents xs = zipWith (++) (nexts xs) (prevs xs)

lookupPoint heights (x,y)
  | x < 0 || y < 0 = 9
  | x >= length heights = 9
  | y >= length (heights !! x) = 9
  | otherwise = heights !! x !! y

adjPoints (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

basinSizeBFS heights seen acc [] = acc
basinSizeBFS heights seen acc (p:ps)
  | p `elem` seen = basinSizeBFS heights seen acc ps
  | lookupPoint heights p < 9 = basinSizeBFS heights (p:seen) (1+acc) $ ps ++ adjPoints p
  | otherwise = basinSizeBFS heights seen acc ps

basinSize heights basin = basinSizeBFS heights [] 0 [basin]

main = do
  heights <- map (map digitToInt) . lines <$> readFile "input"
  let adjMap = zipWith (zipWith (++)) (map adjacents heights) (transpose $ map adjacents $ transpose heights)
  let basins = concat $ zipWith (\(x, l) ms -> catMaybes $ zipWith (\(y, v) m -> if v < m then Just (x,y) else Nothing) l ms) (zip [0..] $ map (zip [0..]) heights) (map (map minimum) adjMap)
  print $ product $ take 3 $ reverse $ sort $ map (basinSize heights) basins
