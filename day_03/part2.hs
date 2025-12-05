import Data.Char (digitToInt)
import System.IO

maxSubsequence :: Int -> [Int] -> [Int]
maxSubsequence k xs = take k $ go xs []
  where
    n = length xs
    go :: [Int] -> [Int] -> [Int]
    go [] stack = stack
    go (d:ds) stack =
      let rem = length (d:ds)
          popWhile st
            | not (null st)
              && last st < d
              && (length st - 1 + rem) >= k = popWhile (init st)
            | otherwise = st
          stack' = popWhile stack
      in if length stack' < k
           then go ds (stack' ++ [d])
           else go ds stack'

bestK :: Int -> String -> Integer
bestK k s =
  let digits = map digitToInt s
      picked = maxSubsequence k digits
  in read (concatMap show picked)

main :: IO ()
main = do
  input <- fmap lines getContents
  let k = 12
      results = map (bestK k) input
  print (sum results)
