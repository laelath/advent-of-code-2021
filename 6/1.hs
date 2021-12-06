splitOn c s =
  case break (==c) s of
    (a, ',':b) -> a : splitOn c b
    (a, "")    -> [a]
    _          -> undefined

nextDay acc [] = acc
nextDay acc (f:fs)
  | f == 0 = nextDay (8 : 6 : acc) fs
  | otherwise = nextDay (f-1 : acc) fs

doNDays n fish
  | n == 0 = fish
  | otherwise = doNDays (n-1) $ nextDay [] fish

main = do
  fish <- map (read :: String -> Int) . splitOn ',' <$> readFile "input"
  print $ length $ doNDays 80 fish
  return ()
