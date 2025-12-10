import Data.List (isPrefixOf)
import System.IO

isRepeated :: String -> Bool
isRepeated s =
    let n = length s
    in or
       [ let chunk = take k s
         in s == concat (replicate (n `div` k) chunk)
       | k <- [1 .. n `div` 2]
       , n `mod` k == 0
       , (n `div` k) >= 2
       ]

parseRange :: String -> (Int, Int)
parseRange str =
    let (a, (_:b)) = break (=='-') str
    in (read a, read b)

splitComma :: String -> [String]
splitComma s =
    case dropWhile (==',') s of
        "" -> []
        s' -> w : splitComma s''
            where (w, s'') = break (==',') s'

main :: IO ()
main = do
    input <- getContents
    let ranges = map parseRange . filter (not . null) . splitComma $ input
        invalids = [ n
                   | (a,b) <- ranges
                   , n <- [a..b]
                   , isRepeated (show n)
                   ]
    print (sum invalids)
