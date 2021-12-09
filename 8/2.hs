import Data.Bifunctor
import Data.List
import Data.Maybe

digits = ["abcdef", "bc", "abdeg", "abcdg", "bcfg", "acdfg", "acdefg", "abc", "abcdefg", "abcdfg"]

findDigit :: String -> Maybe Int
findDigit str = elemIndex (sort str) digits

applyMapping :: [(Char, Char)] -> [String] -> [String]
applyMapping m = map $ map $ fromJust . (`lookup` m)

checkMapping :: [(Char, Char)] -> [String] -> Bool
checkMapping m pats = isJust $ mapM findDigit $ applyMapping m pats

allMappings :: [[(Char, Char)]]
allMappings = map (`zip` "abcdefg") $ permutations "abcdefg"

findMapping :: [String] -> [(Char, Char)]
findMapping pats = head $ filter (`checkMapping` pats) allMappings

toNum :: [Int] -> Int
toNum = foldl' (\x y -> 10*x+y) 0

decode :: [String] -> [String] -> Int
decode pats digits = toNum $ map (fromJust . findDigit) $ applyMapping (findMapping pats) digits

main = do
  input <- map (second tail . break (=="|") . map sort . words) . lines <$> readFile "input"
  print $ sum $ map (uncurry decode) input
