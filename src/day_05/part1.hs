import Data.List (foldl')

parseRange :: String -> (Int, Int)
parseRange line =
    let (a, _ : b) = span (/= '-') line
    in (read a, read b)

inAnyRange :: Int -> [(Int, Int)] -> Bool
inAnyRange x ranges = any (\(lo, hi) -> x >= lo && x <= hi) ranges

main :: IO ()
main = do
    input <- lines <$> getContents
    let (rangeLines, _ : idLines) = break (== "") input
        ranges = map parseRange rangeLines
        ids    = map read idLines

        freshCount = length (filter (`inAnyRange` ranges) ids)

    print freshCount
