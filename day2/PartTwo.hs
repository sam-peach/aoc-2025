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
   in checkPattern str 1

checkPattern :: String -> Int -> Bool
checkPattern str index
  | index > len `div` 2 = False
  | len `mod` index /= 0 = checkPattern str (index + 1)
  | otherwise =
      let groups = groupEvery index str
          allEqual = allSame groups
       in allEqual || checkPattern str (index + 1)
  where
    len = length str

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (== x) xs

groupEvery :: Int -> String -> [String]
groupEvery _ [] = []
groupEvery n str =
  let (first, rest) = splitAt n str
   in first : groupEvery n rest

parseInput :: String -> [(Int, Int)]
parseInput input =
  let parts = splitOn (T.pack ",") (T.pack input)
      toTuple part =
        let [a, b] = map (read . T.unpack) (splitOn (T.pack "-") part)
         in (a, b)
   in map toTuple parts
