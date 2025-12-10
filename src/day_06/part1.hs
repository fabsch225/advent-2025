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
        trimmedRows = map trim rows
        opRow = last trimmedRows
        op = head (filter (`elem` "+*") opRow)
        numberRows = init trimmedRows
        numbers = catMaybes (map parseNum numberRows)
    in (numbers, op)
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    parseNum s =
        let ds = filter isDigit s
        in if null ds then Nothing else Just (read ds)

evalProblem :: ([Integer], Char) -> Integer
evalProblem (nums, op) =
    case op of
      '+' -> sum nums
      '*' -> product nums
      _   -> error "unknown operator"

main :: IO ()
main = do
    input <- lines <$> getContents
    let padded = map (\row -> row ++ replicate 500 ' ') input
        cols = transpose padded
        problemCols = splitIntoProblems cols
        problems = map parseProblem problemCols
        results = map evalProblem problems
    print (sum results)
