import Data.List

splitOn c s =
    case break (==c) s of
    (a, ',':b) -> a : splitOn c b
    (a, "")    -> [a]
    _          -> undefined

gauss n = (n^2 + n) `div` 2

main = do
  crabs <- map (read :: String -> Int) . splitOn ',' <$> readFile "input"
  print $ minimum $ map (\i -> (sum $ map (gauss . abs . subtract i) crabs, i)) [0..(maximum crabs + 1)]
