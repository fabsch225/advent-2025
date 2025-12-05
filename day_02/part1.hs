import Data.List (isPrefixOf)
import Data.Char (digitToInt)
import System.IO

isRepeatedTwice :: Int -> Bool
isRepeatedTwice n =
    let s = show n
        len = length s
    in  len `mod` 2 == 0 &&
        let half = len `div` 2
            (a,b) = splitAt half s
        in a == b

parseRange :: String -> (Int, Int)
parseRange str =
    let (a, (_:b)) = break (=='-') str
    in (read a, read b)

main :: IO ()
main = do
    input <- getContents
    let ranges = map parseRange . filter (not . null) . splitComma $ input
        invalids = [ n
                   | (a,b) <- ranges
                   , n <- [a..b]
                   , isRepeatedTwice n
                   ]
    print (sum invalids)

splitComma :: String -> [String]
splitComma s =
    case dropWhile (==',') s of
        "" -> []
        s' -> w : splitComma s''
            where (w, s'') = break (==',') s'
