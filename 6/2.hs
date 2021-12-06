splitOn c s =
  case break (==c) s of
    (a, ',':b) -> a : splitOn c b
    (a, "")    -> [a]
    _          -> undefined

count v = length . filter (==v)

makePop fish =
  (count 0 fish,
   count 1 fish,
   count 2 fish,
   count 3 fish,
   count 4 fish,
   count 5 fish,
   count 6 fish,
   count 7 fish,
   count 8 fish)

nextDay (a,b,c,d,e,f,g,h,i) = (b,c,d,e,f,g,a+h,i,a)

doNDays n fish
  | n == 0 = fish
  | otherwise = doNDays (n-1) $ nextDay fish

sumPop (a,b,c,d,e,f,g,h,i) = a+b+c+d+e+f+g+h+i

main = do
  fish <- map (read :: String -> Int) . splitOn ',' <$> readFile "input"
  print $ sumPop $ doNDays 256 $ makePop fish
  return ()
