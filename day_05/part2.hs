import Data.List (sortOn)

-- Parse "A-B" into (A,B)
parseRange :: String -> (Int, Int)
parseRange s =
  let (a,_:b) = span (/='-') s
  in (read a, read b)

-- Merge overlapping or adjacent ranges (left-to-right)
mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges rs = reverse $ foldl go [] (sortOn fst rs)
  where
    go [] r = [r]
    go ((s,e):acc) (s',e')
      | s' <= e + 1 = (s, max e e') : acc  -- extend current range
      | otherwise   = (s',e') : (s,e) : acc  -- new range

rangeSize :: (Int, Int) -> Int
rangeSize (a,b) = b - a + 1

main :: IO ()
main = do
  input <- lines <$> getContents
  let (rangeLines, _) = break (== "") input
      ranges = map parseRange rangeLines
      merged = mergeRanges ranges
      totalFresh = sum $ map rangeSize merged
  print totalFresh
