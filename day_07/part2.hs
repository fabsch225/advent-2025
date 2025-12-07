module Main where

import qualified Data.Map.Strict as M
import Data.Array ((!), listArray)
import Data.List (elemIndex)
import System.IO (getContents)

type BeamMap = M.Map Int Integer
type SimulationState = (BeamMap, Integer)

main :: IO ()
main = do
    contents <- getContents
    let rows = lines contents
    print $ solve rows

solve :: [String] -> Integer
solve [] = 0
solve (firstRow:restRows) = totalTimelines
  where
    startCol = case elemIndex 'S' firstRow of
        Just c  -> c
        Nothing -> error "Input has no start position 'S'"
    
    initialState = (M.singleton startCol 1, 0)

    (finalMap, sideExits) = foldl processRow initialState restRows

    bottomExits = sum (M.elems finalMap)
    totalTimelines = sideExits + bottomExits

processRow :: SimulationState -> String -> SimulationState
processRow (currentMap, accumulatedExits) rowStr = 
    M.foldlWithKey step (M.empty, accumulatedExits) currentMap
  where
    width = length rowStr
    rowArr = listArray (0, width - 1) rowStr

    step :: SimulationState -> Int -> Integer -> SimulationState
    step (nextMap, accExits) col count = 
        case rowArr ! col of
            '^' -> 
                let (map1, exit1) = addBeam nextMap (col - 1) count
                    (map2, exit2) = addBeam map1    (col + 1) count
                in (map2, accExits + exit1 + exit2)
            
            _   -> 
                let (map1, exit1) = addBeam nextMap col count
                in (map1, accExits + exit1)

    addBeam :: BeamMap -> Int -> Integer -> (BeamMap, Integer)
    addBeam m c cnt
        | c < 0 || c >= width = (m, cnt)
        | otherwise           = (M.insertWith (+) c cnt m, 0)