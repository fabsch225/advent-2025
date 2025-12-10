import Data.Char (isDigit)
import Data.List (transpose, groupBy)
import Data.Maybe (catMaybes)

isBlankCol :: String -> Bool
isBlankCol = all (== ' ')

splitIntoProblems :: [String] -> [[String]]
splitIntoProblems cols =
    filter (not . all isBlankCol)
    (groupBy (\a b -> not (isBlankCol a || isBlankCol b)) cols)

parseProblem :: [String] -> ([Integer], Char)
parseProblem cols =
    let rows = transpose cols
        h = length rows
        opRow = last rows
        op = head (filter (`elem` "+*") opRow)

        numberRows = init rows
        perCol = transpose numberRows

        numbers = [ read ds
                   | col <- perCol
                   , let ds = filter isDigit col
                   , not (null ds)
                   ]
    in (numbers, op)

evalProblem :: ([Integer], Char) -> Integer
evalProblem (nums, op) =
    case op of
      '+' -> sum nums
      '*' -> product nums
      _   -> error "unknown operator"

main :: IO ()
main = do
    input <- lines <$> getContents
    let maxw = maximum (map length input)
        padded = map (\r -> r ++ replicate (maxw - length r) ' ') input
        cols = transpose padded
        blocks = splitIntoProblems cols
        problems = map parseProblem blocks
        results = map evalProblem problems
    print (sum results)
