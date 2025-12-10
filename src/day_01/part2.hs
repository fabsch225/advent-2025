module Main where

import System.IO (getContents)
import Data.List (mapAccumL)

processStep :: Int -> String -> (Int, Int)
processStep currentPos (dir:amountStr) =
    let amount = read amountStr :: Int
        
        (newPos, clicks) = case dir of
            'R' -> 
                let target = currentPos + amount
                    count = (target `div` 100) - (currentPos `div` 100)
                in (target, count)
                
            'L' -> 
                let target = currentPos - amount
                    count = ((currentPos - 1) `div` 100) - ((target - 1) `div` 100)
                in (target, count)
                
            _ -> (currentPos, 0)

        normalizedPos = newPos `mod` 100
    in 
        (normalizedPos, clicks)

processStep currentPos [] = (currentPos, 0)

solve :: [String] -> Int
solve instructions =
    let 
        startPos = 50
        (_, clickCounts) = mapAccumL processStep startPos instructions
    in 
        sum clickCounts

main :: IO ()
main = do
    input <- getContents
    print (solve (lines input))