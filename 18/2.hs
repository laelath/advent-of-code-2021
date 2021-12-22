import Data.List

import System.Environment
import System.Exit
import System.IO

import Text.Parsec

data SnailNum = SPair SnailNum SnailNum | SNum Int

instance Show SnailNum where
  show (SPair l r) = "[" ++ show l ++ "," ++ show r ++ "]"
  show (SNum i) = show i

parseFromFile p fname = do
  input <- readFile fname
  return (runParser p () fname input)

parseInput fileName = parseFromFile parser fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

parser = parseSnailNum `sepEndBy` newline

parseSnailNum = parseSPair <|> parseSNum
  where
    parseSPair = do
      char '['
      l <- parseSnailNum
      char ','
      r <- parseSnailNum
      char ']'
      return (SPair l r)
    parseSNum = SNum . (read :: String -> Int) <$> many1 digit

addToLeft (SNum i) n = SNum $ i + n
addToLeft (SPair l r) n = SPair (addToLeft l n) r

addToRight (SNum i) n = SNum $ i + n
addToRight (SPair l r) n = SPair l (addToRight r n)

explode :: SnailNum -> Maybe SnailNum
explode num = case explode' 0 num of
  Nothing -> Nothing
  Just (num', _, _) -> Just num'
  where
    explode' _ (SNum _) = Nothing
    explode' 4 (SPair (SNum l) (SNum r)) = Just (SNum 0, Just l, Just r)
    explode' n (SPair l r) =
      case explode' (n + 1) l of
        Just (l', ml, mr) -> Just (SPair l' (maybe r (addToLeft r) mr), ml, Nothing)
        Nothing ->
          case explode' (n + 1) r of
            Just (r', ml, mr) -> Just (SPair (maybe l (addToRight l) ml) r', Nothing, mr)
            Nothing -> Nothing

split :: SnailNum -> Maybe SnailNum
split (SNum i)
  | i > 9 = Just $ SPair (SNum $ i `div` 2) (SNum $ (i + 1) `div` 2)
  | otherwise = Nothing
split (SPair l r) =
  case split l of
    Just l' -> Just $ SPair l' r
    Nothing ->
      case split r of
        Just r' -> Just $ SPair l r'
        Nothing -> Nothing

reduce :: SnailNum -> SnailNum
reduce num = case explode num of
  Just num' -> reduce num'
  Nothing -> maybe num reduce (split num)

snailAdd sn1 sn2 = reduce $ SPair sn1 sn2

magnitude (SNum i) = i
magnitude (SPair l r) = 3 * magnitude l + 2 * magnitude r

pairs :: [a] -> [(a, a)]
pairs l = concat [[(x,y), (y,x)] | (x:ys) <- tails l, y <- ys]

main = do
  args <- getArgs
  nums <- parseInput (head args)
  print $ maximum $ map (magnitude . uncurry snailAdd) $ pairs nums
