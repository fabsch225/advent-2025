import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Int (Int64)
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import System.IO
import Data.STRef

parseLine :: String -> (Int64,Int64,Int64)
parseLine s =
  case map read (split s) of
    [x,y,z] -> (x,y,z)
    _       -> error $ "bad line: " ++ s
  where
    split "" = []
    split str =
      let (a,rest) = break (==',') str
      in case rest of
           []      -> [a]
           (_:rs)  -> a : split rs

sqDist :: (Int64,Int64,Int64) -> (Int64,Int64,Int64) -> Int64
sqDist (x1,y1,z1) (x2,y2,z2) =
  let dx = x1 - x2
      dy = y1 - y2
      dz = z1 - z2
  in dx*dx + dy*dy + dz*dz

allEdges :: [(Int64,Int64,Int64)] -> [(Int64,Int,Int)]
allEdges pts = go 0 pts
  where
    go _ []     = []
    go i (p:ps) =
      [ (sqDist p q, i, j)
      | (q,j) <- zip ps [(i+1)..]
      ] ++ go (i+1) ps

lastConnectingEdge :: Int -> [(Int64,Int,Int)] -> (Int,Int)
lastConnectingEdge n edges = runST $ do
  parent <- newArray (0,n-1) 0 :: ST s (STUArray s Int Int)
  sizeA  <- newArray (0,n-1) 1 :: ST s (STUArray s Int Int)
  forM_ [0..n-1] $ \i -> writeArray parent i i

  comps <- newSTRef n

  let findRoot x = do
        p <- readArray parent x
        if p == x then return x else do
          r <- findRoot p
          writeArray parent x r
          return r

      union a b = do
        ra <- findRoot a
        rb <- findRoot b
        if ra == rb
          then return False
          else do
            sa <- readArray sizeA ra
            sb <- readArray sizeA rb
            if sa >= sb
              then do writeArray parent rb ra
                      writeArray sizeA ra (sa + sb)
              else do writeArray parent ra rb
                      writeArray sizeA rb (sa + sb)
            return True

  let loop [] = error "Ran out of edges before full connectivity"
      loop ((_,i,j):rest) = do
        merged <- union i j
        when merged $ do
          c <- readSTRef comps
          writeSTRef comps (c - 1)
        c <- readSTRef comps
        if c == 1 && merged
          then return (i,j)
          else loop rest

  loop edges

main :: IO ()
main = do
  input <- getContents
  let ls   = filter (not . null) $ map (filter (/= '\r')) $ lines input
      pts  = map parseLine ls
      n    = length pts
      edges = allEdges pts
      sorted = sortBy (comparing (\(d,_,_) -> d)) edges

  let (i,j) = lastConnectingEdge n sorted
      (x1,_,_) = pts !! i
      (x2,_,_) = pts !! j

  print (x1 * x2)
