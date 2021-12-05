readCmd :: [String] -> (Int, Int)
readCmd ["forward", n] = (read n, 0)
readCmd ["up", n] = (0, -(read n))
readCmd ["down", n] = (0, read n)

calcAims :: Int -> [(Int,Int)] -> [(Int, Int)]
calcAims acc [] = []
calcAims acc ((0,v) : rst) = calcAims (acc + v) rst
calcAims acc ((h,0) : rst) = (h,acc) : calcAims acc rst

main = do
  input <- readFile "input"
  let cmds = map (readCmd . words) (lines input)
  let loc = foldl (\(x,y) (a,b) -> (x+a,y+b)) (0,0) cmds
  print $ fst loc * snd loc

  let loc2 = (sum (map fst cmds), sum (map (uncurry (*)) (calcAims 0 cmds)))
  print $ fst loc2 * snd loc2
