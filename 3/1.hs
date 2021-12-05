import Data.List

countOcc c = foldl (\n d -> if c == d then 1 + n else n) 0

readBin [] = 0
readBin ('0' : s) = readBin s
readBin ('1' : s) = 2 ^ length s + readBin s
readBin _ = undefined

main = do
  input <- readFile "input"
  let bits = transpose $ lines input
      counts = zip (map (countOcc '0') bits) (map (countOcc '1') bits)
      gamma_bits = map (\(n0,n1) -> if n0 > n1 then '0' else '1') counts
      epsilon_bits = map (\(n0,n1) -> if n1 > n0 then '0' else '1') counts

  print $ readBin gamma_bits * readBin epsilon_bits
