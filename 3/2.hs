import Data.List

countOcc c = foldl (\n d -> if c == d then 1 + n else n) 0

findNum :: (Integer -> Integer -> Bool) -> [String] -> String
findNum cmp [] = undefined
findNum cmp [s] = s
findNum cmp strs =
  let bits = map head strs
      bit = if cmp (countOcc '0' bits) (countOcc '1' bits) then '0' else '1'
  in
    bit : findNum cmp (map tail $ filter (\s -> head s == bit) strs)

readBin [] = 0
readBin ('0' : s) = readBin s
readBin ('1' : s) = 2 ^ length s + readBin s
readBin _ = undefined

main = do
  input <- readFile "input"
  let bits = lines input
      oxgn = findNum (>) bits
      scbr = findNum (<=) bits

  print $ readBin oxgn * readBin scbr
