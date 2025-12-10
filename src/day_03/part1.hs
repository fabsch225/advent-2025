import Data.Char (digitToInt)
import System.IO
import Data.Maybe (fromJust)

maxTwoDigit :: String -> Int
maxTwoDigit s =
    let indexed = zip [0..] (map digitToInt s)

        firstIndex d = lookup d
            [ (digit, idx)
            | (idx, digit) <- indexed
            ]
        candidates =
            [ 10 * d + bestAfter
            | d <- [9,8..0]
            , Just i <- [firstIndex d]
            , let after = [v | (j,v) <- indexed, j > i]
            , not (null after)
            , let bestAfter = maximum after
            ]
    in maximum candidates

main :: IO ()
main = do
    input <- fmap lines getContents
    let results = map maxTwoDigit input
    print (sum results)
