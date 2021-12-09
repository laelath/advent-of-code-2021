import Data.Char
import Data.List

nexts [] = []
nexts (x:xs) = take 1 xs : nexts xs
prevs = reverse . nexts . reverse

adjacents xs = zipWith (++) (nexts xs) (prevs xs)

main = do
  heights <- map (map digitToInt) . lines <$> readFile "input"
  let adjMap = zipWith (zipWith (++)) (map adjacents heights) (transpose $ map adjacents $ transpose heights)
  print $ sum $ concat $ zipWith (zipWith (\x m -> if x < m then x + 1 else 0)) heights (map (map minimum) adjMap)
