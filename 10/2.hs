import Data.List
import Data.Maybe

chunkOpen c = c == '(' || c == '[' || c == '{' || c == '<'
chunkClose c = c == ')' || c == ']' || c == '}' || c == '>'

closes c o = o == '(' && c == ')' || o == '[' && c == ']' || o == '{' && c == '}' || o == '<' && c == '>'

complete ctx [] = Just ctx
complete ctx (h:t)
  | chunkOpen h = complete (h : ctx) t
  | chunkClose h =
    case ctx of
      [] -> Nothing
      c:r -> if h `closes` c then complete r t else Nothing
  | otherwise = undefined


score = foldl (\acc c -> acc * 5 + scoreChar c)
  where
    scoreChar c =
      case c of
      '(' -> 1
      '[' -> 2
      '{' -> 3
      '<' -> 4
      _ -> undefined

main = do
  input <- lines <$> readFile "input"
  let scores = sort $ map (score 0) $ mapMaybe (complete []) input
  print $ scores !! (length scores `div` 2)
