countInc l = foldl (\acc (n,m) -> if n >= m then acc else acc + 1) 0 (zip l (tail l))

main = do
  input <- readFile "input"
  let dists = map read (lines input) :: [Int]
  print $ countInc dists

  print $ countInc $ zipWith3 (\x y z -> x + y + z) (tail (tail dists)) (tail dists) dists
