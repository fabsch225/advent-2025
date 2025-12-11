import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')
import System.IO (getContents)

type Device = String
type Graph = Map Device [Device]

parseInput :: String -> Graph
parseInput input =
    Map.fromList $ map parseLine (lines input)
  where
    parseLine :: String -> (Device, [Device])
    parseLine line =
        case words $ filter (/= ':') line of
            (name : outputs) -> (name, outputs)
            _                -> error $ "Invalid line format: " ++ line

countPaths' :: Graph -> Device -> Device -> Integer
countPaths' graph startNode endNode =
    let
        go :: Map Device Integer -> Device -> (Map Device Integer, Integer)
        go currentMemo device
            | Just count <- Map.lookup device currentMemo = (currentMemo, count)
            | device == endNode = (Map.insert device 1 currentMemo, 1)
            | otherwise =
                case Map.lookup device graph of
                    Nothing -> (Map.insert device 0 currentMemo, 0)
                    Just outputs ->
                        let
                            (finalMemo, totalPaths) = foldl'
                                (\(m, acc) outputDevice ->
                                    let
                                        (newMemo, paths) = go m outputDevice
                                    in
                                        (newMemo, acc + paths)
                                )
                                (currentMemo, 0)
                                outputs
                        in
                            (Map.insert device totalPaths finalMemo, totalPaths)
    in
        let (_, finalCount) = go Map.empty startNode
        in finalCount

solvePart2 :: Graph -> Integer
solvePart2 graph =
    let
        paths_1 =
            (countPaths' graph "svr" "dac") *
            (countPaths' graph "dac" "fft") *
            (countPaths' graph "fft" "out")

        paths_2 =
            (countPaths' graph "svr" "fft") *
            (countPaths' graph "fft" "dac") *
            (countPaths' graph "dac" "out")
    in
        paths_1 + paths_2

main :: IO ()
main = do
    input <- getContents
    let graph = parseInput input
        result = solvePart2 graph
    print result