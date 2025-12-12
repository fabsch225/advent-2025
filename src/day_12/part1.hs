{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.List (isSuffixOf, elemIndices)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Region = Region
  { width  :: Int
  , length_ :: Int
  , shapesUsed :: [Int]
  } deriving (Show)

type Shape = [(Int, Int)]
type Shapes = M.Map Int Shape

couldFit :: Shapes -> Region -> Bool
couldFit shapes r =
    let area = width r * length_ r
        totalShapeCells =
            sum [ count * shapeSize s
                | (s, count) <- zip [0..] (shapesUsed r)
                , count > 0
                ]
        shapeSize s = maybe 0 length (M.lookup s shapes)
    in area >= totalShapeCells


parseInput :: [T.Text] -> (Shapes, [Region])
parseInput blocks = foldl parseBlock (M.empty, []) blocks
  where
    parseBlock (shMap, regions) block =
        let ls = T.lines block
            header = head ls
        in if T.isSuffixOf ":" header
           then
               let idx = read (T.unpack (T.init header)) :: Int
                   shapePoints =
                       [ (r,c)
                       | (r, line) <- zip [0..] (tail ls)
                       , (c, ch)  <- zip [0..] (T.unpack line)
                       , ch == '#'
                       ]
               in (M.insert idx shapePoints shMap, regions)

           else
               let newRegions = map parseRegionLine ls
               in (shMap, regions ++ newRegions)

    parseRegionLine :: T.Text -> Region
    parseRegionLine line =
        let
            nums = map (read . T.unpack) $ T.words $ T.map (\c -> if T.elem c (T.pack "0123456789") then c else ' ') line
            w = nums !! 0
            l = nums !! 1
            shapeCounts = drop 2 nums
        in Region w l shapeCounts

main :: IO ()
main = do
    input <- TIO.getContents
    let blocks = T.splitOn "\n\n" (T.strip input)
    let (shapes, regions) = parseInput blocks

    let answer = length (filter (couldFit shapes) regions)
    print answer
