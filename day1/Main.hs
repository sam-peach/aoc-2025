dialBase :: Int
dialBase = 100

dialStart :: Int
dialStart = 50

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let nums = map parseLine ls
  let res = scanl applyRotation dialStart nums
  print (show (countZeros res))

parseLine :: String -> Int
parseLine (c : rest) = case c of
  'R' -> read rest :: Int
  'L' -> (read rest :: Int) * (-1)
  _ -> error "Invalid line"

applyRotation :: Int -> Int -> Int
applyRotation num state = (state + num) `mod` dialBase

countZeros :: [Int] -> Int
countZeros xs = length (filter (== 0) xs)