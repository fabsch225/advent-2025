import System.IO
import Data.Maybe (catMaybes)

type Grid = [String]
type Pos  = (Int, Int)

neighbors :: [ (Int, Int) ]
neighbors = [ (dx,dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0) ]

inBounds :: Grid -> Pos -> Bool
inBounds g (r,c) = r >= 0 && r < length g && c >= 0 && c < length (g !! 0)

get :: Grid -> Pos -> Maybe Char
get g p@(r,c)
  | inBounds g p = Just ((g !! r) !! c)
  | otherwise    = Nothing

adjAtSigns :: Grid -> Pos -> Int
adjAtSigns g (r,c) =
  length [ () | (dx,dy) <- neighbors
              , let p = (r+dx, c+dy)
              , get g p == Just '@' ]

isAccessible :: Grid -> Pos -> Bool
isAccessible g p = get g p == Just '@' && adjAtSigns g p < 4

allPositions :: Grid -> [Pos]
allPositions g =
  let rows = length g
      cols = if rows == 0 then 0 else length (g !! 0)
  in [ (r,c) | r <- [0..rows-1], c <- [0..cols-1] ]

main :: IO ()
main = do
  input <- fmap lines getContents
  let grid = filter (not . null) input
      accessibleCount = length $ filter (isAccessible grid) (allPositions grid)
  print accessibleCount
