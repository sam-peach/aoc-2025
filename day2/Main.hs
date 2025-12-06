import Data.Text (Text, splitOn)
import Data.Text qualified as T

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ranges = parseInput input
  let symmetricalNumbers = process ranges
  let result = sum symmetricalNumbers
  print result

process :: [(Int, Int)] -> [Int]
process ranges = [n | (start, end) <- ranges, n <- [start .. end], hasPattern n]

hasPattern :: Int -> Bool
hasPattern n =
  let str = show n
      len = length str
   in even len
        && let (left, right) = splitAt (len `div` 2) str
            in left == right

parseInput :: String -> [(Int, Int)]
parseInput input =
  let parts = splitOn (T.pack ",") (T.pack input)
      toTuple part =
        let [a, b] = map (read . T.unpack) (splitOn (T.pack "-") part)
         in (a, b)
   in map toTuple parts
