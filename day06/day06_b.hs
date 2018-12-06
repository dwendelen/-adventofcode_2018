import System.IO
import Text.Regex.Posix
import Data.List (nub, maximumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

draw = (0,0)
initialOwner = (draw, 99999999)

inputFile = "input.txt"
limit = 10000

--inputFile = "example.txt"
--limit = 32

calc :: [(Integer, Integer)] -> Integer
calc list =
  let
    gridSize = calculateGridSize list
    score = calcScore list gridSize
  in
    score

calculateGridSize list =
  let
      minX = minimum (map fst list)
      maxX = maximum (map fst list)
      minY = minimum (map snd list)
      maxY = maximum (map snd list)

      distPerNode = (div limit  (toInteger (length list))) + 1
   in
      ((minX - distPerNode, maxX + distPerNode), (minY - distPerNode, maxY + distPerNode))

calcScore list gridSize =
  let
    grid = getGrid gridSize
    score = foldl (incScore list) 0 grid
  in
    score


incScore ::   [(Integer, Integer)] ->
              Integer ->
              (Integer, Integer) ->
              Integer
incScore list acc cell =
  if calcCellScore list cell < limit
    then acc + 1
    else acc


calcCellScore :: [(Integer, Integer)] -> (Integer, Integer) -> Integer
calcCellScore list cell =
  foldl (\acc node -> acc + dist cell node) 0 list


getGrid ((minX, maxX), (minY, maxY)) =
  [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]




-- Util
main :: IO ()
main = do
  file <- readFile inputFile;
  let
    codes = lines file
    parsed = map parse codes
    result = calc parsed
  print result



parse :: String -> (Integer, Integer)
parse string =
  let
    (_,_,_, match) = string =~ "([0-9]+), ([0-9]+)" :: (String, String, String, [String])
    x = read (match!!0) :: Integer
    y = read (match!!1) :: Integer
  in
    (x, y)

dist (a, b) (c, d) =
  (abs (a - c)) + (abs (b - d))

cross :: [a] -> [(a, a)]
cross list =
  concatMap (\i -> map(\j -> (i, j)) list) list