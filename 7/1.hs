import Data.List

splitOn c s =
    case break (==c) s of
    (a, ',':b) -> a : splitOn c b
    (a, "")    -> [a]
    _          -> undefined

main = do
  crabs <- map (read :: String -> Int) . splitOn ',' <$> readFile "input"
  print $ minimum $ map (\i -> (sum $ map (abs . subtract i) crabs, i)) [0..maximum crabs]
