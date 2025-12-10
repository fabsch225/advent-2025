import Data.Array
import Data.List (sortBy, foldl')
import Data.Ord (comparing, Down(..))

type Point = (Int, Int)

type Edge = (Point, Point)

data Rect = Rect 
  { rMinX :: Int
  , rMaxX :: Int
  , rMinY :: Int
  , rMaxY :: Int 
  } deriving (Show, Eq)

main :: IO ()
main = do
    input <- getContents
    let points = parseInput input
    let result = solve points
    print result

parseInput :: String -> Array Int Point
parseInput str = listArray (0, length pts - 1) pts
  where
    pts = map parseLine (lines str)
    parseLine :: String -> Point
    parseLine s = 
        let (xStr, rest) = break (== ',') s
            yStr = drop 1 rest
        in (read xStr, read yStr)

solve :: Array Int Point -> Int
solve points = maxArea
  where
    (_, nMinus1) = bounds points
    n = nMinus1 + 1
    
    edges :: [Edge]
    edges = [ (points ! i, points ! ((i + 1) `mod` n)) | i <- [0 .. nMinus1] ]

    pairCandidates = 
        [ (points ! i, points ! j) 
        | i <- [0 .. nMinus1], j <- [i + 1 .. nMinus1] ]

    sortedPairs = sortBy (comparing (Down . uncurry calcArea)) pairCandidates

    maxArea = case dropWhile (not . isValid edges) sortedPairs of
        [] -> 0 
        ((p1, p2):_) -> calcArea p1 p2

calcArea :: Point -> Point -> Int
calcArea (x1, y1) (x2, y2) = 
    (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

isValid :: [Edge] -> (Point, Point) -> Bool
isValid edges (p1, p2) = 
    let rect = makeRect p1 p2
        cx2 = rMinX rect + rMaxX rect
        cy2 = rMinY rect + rMaxY rect
    in not (intersectsBoundary edges rect) && isInsideOrOnBoundary edges cx2 cy2

makeRect :: Point -> Point -> Rect
makeRect (x1, y1) (x2, y2) = Rect 
    (min x1 x2) (max x1 x2) 
    (min y1 y2) (max y1 y2)

intersectsBoundary :: [Edge] -> Rect -> Bool
intersectsBoundary edges (Rect xL xR yB yT) = any cuts edges
  where
    cuts ((ax, ay), (bx, by))
      | ax == bx = 
          let (ey1, ey2) = (min ay by, max ay by)
          in (ax > xL && ax < xR) && (max yB ey1 < min yT ey2)
      | ay == by = 
          let (ex1, ex2) = (min ax bx, max ax bx)
          in (ay > yB && ay < yT) && (max xL ex1 < min xR ex2)
      | otherwise = False 

isInsideOrOnBoundary :: [Edge] -> Int -> Int -> Bool
isInsideOrOnBoundary edges px2 py2 = onBoundary || (odd crossings)
  where
    (onBoundary, crossings) = foldl' step (False, 0) edges

    step (isEdge, count) ((ax, ay), (bx, by))
        | isEdge = (True, count)
        | otherwise =
            let ax2 = ax * 2; ay2 = ay * 2
                bx2 = bx * 2; by2 = by * 2
                
                onSegment = 
                    if ax2 == bx2 && ax2 == px2 
                    then py2 >= min ay2 by2 && py2 <= max ay2 by2
                    else if ay2 == by2 && ay2 == py2
                    then px2 >= min ax2 bx2 && px2 <= max ax2 bx2
                    else False
                
                isCrossing = 
                    if ax2 == bx2
                    then 
                        let (yMin, yMax) = (min ay2 by2, max ay2 by2)
                        in ax2 > px2 && (py2 >= yMin && py2 < yMax)
                    else False

            in (onSegment, if isCrossing then count + 1 else count)