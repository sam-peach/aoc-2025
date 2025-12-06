main :: IO ()
main = do
  input <- readFile "input.txt"
  let linesOfInput = lines input
  let res = map calcJoltage linesOfInput
  print $ sum res

calcJoltage :: String -> Int
calcJoltage line =
  let (a, b) = findJoltagePair line 0 0
   in read (show a ++ show b) :: Int

findJoltagePair :: String -> Int -> Int -> (Int, Int)
findJoltagePair "" max1 max2 = (max1, max2)
findJoltagePair (c : rest) max1 max2
  | val > max1 && restLen >= 1 =
      findJoltagePair rest val 0
  | val > max2 =
      findJoltagePair rest max1 val
  | otherwise = findJoltagePair rest max1 max2
  where
    restLen = length rest
    val = read [c] :: Int
