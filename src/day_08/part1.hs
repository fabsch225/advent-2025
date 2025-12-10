import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Int (Int64)
import Data.List (sortBy)
import Data.List qualified as L
import Data.Ord (comparing)
import System.IO

parseLine :: String -> (Int64, Int64, Int64)
parseLine s =
  case map read (splitComma s) of
    [x, y, z] -> (x, y, z)
    _ -> error $ "bad line: " ++ s
  where
    splitComma :: String -> [String]
    splitComma "" = []
    splitComma str =
      let (a, rest) = break (== ',') str
       in case rest of
            [] -> [a]
            (_ : rs) -> a : splitComma rs

sqDist :: (Int64, Int64, Int64) -> (Int64, Int64, Int64) -> Int64
sqDist (x1, y1, z1) (x2, y2, z2) =
  let dx = x1 - x2
      dy = y1 - y2
      dz = z1 - z2
   in dx * dx + dy * dy + dz * dz

allEdges :: [(Int64, Int64, Int64)] -> [(Int64, Int, Int)]
allEdges pts = go 0 pts
  where
    go _ [] = []
    go i (p : ps) =
      [(sqDist p q, i, j) | (q, j) <- zip ps [i + 1 ..]] ++ go (i + 1) ps

processEdges :: Int -> [(Int64, Int, Int)] -> Int -> [Int]
processEdges n edges takeK = runST $ do
  parent <- newArray (0, n - 1) 0 :: ST s (STUArray s Int Int)
  sizeA <- newArray (0, n - 1) 1 :: ST s (STUArray s Int Int)
  forM_ [0 .. n - 1] $ \i -> writeArray parent i i

  let findRoot x = do
        p <- readArray parent x
        if p == x
          then return x
          else do
            r <- findRoot p
            writeArray parent x r
            return r

      union a b = do
        ra <- findRoot a
        rb <- findRoot b
        if ra == rb
          then return ()
          else do
            sa <- readArray sizeA ra
            sb <- readArray sizeA rb
            if sa >= sb
              then do
                writeArray parent rb ra
                writeArray sizeA ra (sa + sb)
              else do
                writeArray parent ra rb
                writeArray sizeA rb (sa + sb)

  let toProcess = take (min takeK (length edges)) edges
  forM_ toProcess $ \(_, i, j) -> union i j

  roots <- filterM (\i -> (== i) <$> readArray parent i) [0 .. n - 1]
  sizes <- mapM (\r -> readArray sizeA r) roots
  return sizes

main :: IO ()
main = do
  input <- getContents
  let ls = filter (not . null) $ map (filter (/= '\r')) $ lines input
      pts = map parseLine ls
      n = length pts
      edges = allEdges pts
      sorted = sortBy (comparing (\(d, _, _) -> d)) edges
      sizes = processEdges n sorted 1000
      top3 = take 3 $ reverse $ L.sort sizes
      padded = top3 ++ replicate (max 0 (3 - length top3)) 1
      productTop3 = product padded
  print productTop3
