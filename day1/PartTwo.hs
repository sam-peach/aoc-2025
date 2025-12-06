import Data.ByteString (count)
import Data.Type.Equality (apply)

dialBase :: Int
dialBase = 100

dialStart :: Int
dialStart = 50

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let nums = map parseLine ls
  let dialHistory = applyRotation nums dialStart 0
  print $ dialHistory

parseLine :: String -> Int
parseLine (c : rest) = case c of
  'R' -> read rest :: Int
  'L' -> (read rest :: Int) * (-1)
  _ -> error "Invalid line"

applyRotation :: [Int] -> Int -> Int -> (Int, Int)
applyRotation [] state zeroCount = (state, zeroCount)
applyRotation (delta : rest) state zeroCount =
  let newState = (state + delta) `mod` dialBase
      crossZeroCount = countZeroCrossings state delta
      newCount = zeroCount + crossZeroCount
   in applyRotation rest newState newCount

countZeroCrossings :: Int -> Int -> Int
countZeroCrossings state delta
  | delta == 0 = 0
  | steps < firstHit = 0
  | otherwise = 1 + (steps - firstHit) `div` dialBase
  where
    steps = abs delta

    offset
      | delta > 0 = (-state) `mod` dialBase
      | otherwise = state `mod` dialBase

    firstHit =
      if offset == 0
        then dialBase
        else offset
