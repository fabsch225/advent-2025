import qualified Data.Set as S
import Data.Set (Set)
import System.IO
import Data.List (foldl')
import Control.Monad (forM_)

type Pos = (Int, Int)

neighbors :: [Pos]
neighbors = [ (dr,dc) | dr <- [-1..1], dc <- [-1..1], (dr,dc) /= (0,0) ]

add :: Pos -> Pos -> Pos
add (r,c) (dr,dc) = (r+dr, c+dc)

countNeighbors :: Set Pos -> Pos -> Int
countNeighbors s p = length [ () | d <- neighbors, S.member (add p d) s ]

totalRemoved :: Set Pos -> Int
totalRemoved initial = go initial 0
  where
    go :: Set Pos -> Int -> Int
    go s acc =
      let toRemove = S.filter (\p -> countNeighbors s p < 4) s
      in if S.null toRemove
           then acc
           else go (s `S.difference` toRemove) (acc + S.size toRemove)

buildSet :: [String] -> Set Pos
buildSet ls =
  foldl' (\acc (r, row) ->
            foldl' (\a (c,ch) -> if ch == '@' then S.insert (r,c) a else a)
                   acc
                   (zip [0..] row)
         ) S.empty
         (zip [0..] ls)

main :: IO ()
main = do
  input <- fmap (filter (not . null) . lines) getContents
  let s = buildSet input
      result = totalRemoved s
  print result
