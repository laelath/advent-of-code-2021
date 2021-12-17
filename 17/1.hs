import System.Environment
import System.Exit
import System.IO

import Text.Parsec

parseFromFile p fname = do
  input <- readFile fname
  return (runParser p () fname input)

parseInput fileName = parseFromFile parser fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

parser = do
  string "target area: x="
  xLow <- parseInt
  string ".."
  xHigh <- parseInt
  string ", y="
  yLow <- parseInt
  string ".."
  yHigh <- parseInt
  return ((xLow, xHigh), (yLow, yHigh))
  where
    parseInt :: Parsec [Char] a Int
    parseInt = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

reaches v (min, max) =
  min <= stop && stop <= max
  where
    stop = sum [1..v]

stepsInto (dX, dY) ((xMin, xMax), (yMin, yMax)) =
  step 0 0 dX dY
  where
    step x y dX dY
      | xMin <= x && x <= xMax && yMin <= y && y <= yMax = True
      | y < yMin && dY < 0 = False
      | otherwise = step (x + dX) (y + dY) dX' (dY - 1)
      where
        dX' = if dX == 0 then 0 else dX - 1

main = do
  args <- getArgs
  (xRange, yRange) <- parseInput (head args)
  let dX = head [dX | dX <- [1..], dX `reaches` xRange ]
      dY = maximum [dY | dY <- [1..500], (dX, dY) `stepsInto` (xRange, yRange)]
  print $ sum [1..dY]
