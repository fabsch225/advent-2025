module Main where

import qualified Data.Set as S
import Data.Array ((!), listArray, bounds)
import Data.List (elemIndex)
import System.IO (getContents)

type ActiveBeams = S.Set Int
type SimulationState = (ActiveBeams, Int)

main :: IO ()
main = do
    contents <- getContents
    let rows = lines contents
    print $ solve rows

solve :: [String] -> Int
solve [] = 0
solve (firstRow:restRows) = totalSplits
  where
    startCol = case elemIndex 'S' firstRow of
        Just c  -> c
        Nothing -> error "Input has no start position 'S'"
    
    initialBeams = S.singleton startCol
    (_, totalSplits) = foldl processRow (initialBeams, 0) restRows

processRow :: SimulationState -> String -> SimulationState
processRow (currentBeams, count) rowStr = 
    S.foldl' updateBeam (S.empty, count) currentBeams
  where
    width = length rowStr
    rowArr = listArray (0, width - 1) rowStr

    updateBeam :: SimulationState -> Int -> SimulationState
    updateBeam (nextBeams, accCount) col
        | col < 0 || col >= width = (nextBeams, accCount)
        | otherwise = 
            case rowArr ! col of
                '^' -> (S.insert (col - 1) $ S.insert (col + 1) nextBeams, accCount + 1)
                _   -> (S.insert col nextBeams, accCount)