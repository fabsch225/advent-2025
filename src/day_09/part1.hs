import Data.List (maximum)
import System.IO (getContents)

type Coord = (Int, Int)

parseLine :: String -> Coord
parseLine s =
  case break (== ',') s of
    (sx, ',' : sy) -> (read sx, read sy)
    _ -> error "Invalid coordinate format"

parseInput :: String -> [Coord]
parseInput input = map parseLine (lines input)

calculateArea :: Coord -> Coord -> Int
calculateArea (x1, y1) (x2, y2) =
  let width = abs (x1 - x2) + 1
      height = abs (y1 - y2) + 1
   in width * height

solve :: [Coord] -> Int
solve redTiles =
  if length redTiles < 2
    then 0
    else maximum [calculateArea tile1 tile2 | tile1 <- redTiles, tile2 <- redTiles]

main :: IO ()
main = do
  inputString <- getContents
  let redTiles = parseInput inputString
  let maxArea = solve redTiles
  print maxArea