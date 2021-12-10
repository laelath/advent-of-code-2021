import Data.Maybe

chunkOpen c = c == '(' || c == '[' || c == '{' || c == '<'
chunkClose c = c == ')' || c == ']' || c == '}' || c == '>'

closes c o = o == '(' && c == ')' || o == '[' && c == ']' || o == '{' && c == '}' || o == '<' && c == '>'

findIncorrect _ [] = Nothing
findIncorrect ctx (h:t)
  | chunkOpen h = findIncorrect (h : ctx) t
  | chunkClose h =
    case ctx of
      [] -> Nothing
      c:r -> if h `closes` c then findIncorrect r t else Just h
  | otherwise = undefined

score [] = 0
score (h:t) =
  score t +
  case h of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _ -> undefined

main = do
  input <- lines <$> readFile "input"
  print $ score $ mapMaybe (findIncorrect []) input
