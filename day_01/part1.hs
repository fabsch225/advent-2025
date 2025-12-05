module Main where

import System.IO (getContents)

rotate :: Int -> String -> Int
rotate currentPos (direction:amountStr) =
    let amount = read amountStr :: Int
    in case direction of
        'R' -> (currentPos + amount) `mod` 100
        'L' -> (currentPos - amount) `mod` 100
        _   -> currentPos
rotate currentPos [] = currentPos

solve :: [String] -> Int
solve instructions =
    let 
        startPos = 50
        allPositions = scanl rotate startPos instructions
        
        positionsAfterRotation = tail allPositions
    in 
        length $ filter (== 0) positionsAfterRotation

main :: IO ()
main = do
    input <- getContents
    
    let rotations = lines input
    
    print (solve rotations)