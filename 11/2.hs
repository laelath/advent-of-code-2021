import Data.Char
import qualified Data.HashMap as HM

coords :: Int -> [[a]] -> [(Int,Int)]
coords _ [] = []
coords n (h:t) = [(n,m) | m <- [0..length h - 1]] ++ coords (n+1) t

adjPoints :: (Int,Int) -> [(Int,Int)]
adjPoints (x,y) = tail [(x+n,y+m) | n <- [0,1,-1], m <- [0,1,-1]]

updatePoints [] octs = octs
updatePoints (h:t) octs
  | HM.lookup h octs == Just (Just 9) = updatePoints (adjPoints h ++ t) $ HM.adjust (const Nothing) h octs
  | otherwise = updatePoints t $ HM.adjust (fmap (+1)) h octs

countNothings =
  HM.fold (\v acc -> case v of
                       Nothing -> acc + 1
                       Just _ -> acc) 0

tick cs octs =
  let octs' = updatePoints cs octs in
    (countNothings octs', HM.map (\v -> case v of
                                          Nothing -> Just 0
                                          Just n -> Just n) octs')

toHashMap octs = HM.fromList $ zip (coords 0 octs) (map Just (concat octs))

doTicks cs 0 octs = (0, octs)
doTicks cs n octs =
  let (m,octs') = doTicks cs (n-1) octs in
  let (o,octs'') = tick cs octs' in
    (m + o, octs'')

findSync cs n octs =
  let (m,octs') = tick cs octs in
  if m == HM.size octs then
    n+1
  else
    findSync cs (n+1) octs'

main = do
  input <- map (map digitToInt) . lines <$> readFile "input"
  let cs = coords 0 input
      octs = toHashMap input
  -- print $ fst $ doTicks cs 100 octs
  print $ findSync cs 0 octs
