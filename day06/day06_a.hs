import System.IO
import Text.Regex.Posix
import Data.List (nub, maximumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

draw = (0,0)
initialOwner = (draw, 99999999)

calc :: [(Integer, Integer)] ->((Integer, Integer), Integer)
calc list =
  let
    gridSize = calculateGridSize list
    infinites = calcInfiniteOwners list gridSize
    scores = calcScores list gridSize
    filtered = Map.filterWithKey (\owner _ -> not (elem owner infinites)) scores
  in
    maximumBy (comparing snd) (Map.toList filtered)


calculateGridSize list =
  let
      minX = minimum (map fst list)
      maxX = maximum (map fst list)
      minY = minimum (map snd list)
      maxY = maximum (map snd list)

      sizeX = maxX - minX
      sizeY = maxY - minY
      size = maximum (sizeX: sizeY:[])
   in
      ((minX - size, maxX + size), (minY - size, maxY + size))

calcInfiniteOwners list ((minX, maxX), (minY, maxY)) =
  let
    top =     [(x, maxY) | x <- [minX .. maxX]]
    bottom =  [(x, minY) | x <- [minX .. maxX]]
    left =    [(minX, y) | y <- [minY .. maxY]]
    right =   [(maxX, y) | y <- [minY .. maxY]]
    border = concat [top, bottom, left, right]
    owners = map (calcOwner list) border
    unique = nub (draw:owners)
  in
    unique


calcScores list gridSize =
  let
    grid = getGrid gridSize
    scoreMap = foldl (incScore list) Map.empty grid
  in
    scoreMap


incScore ::   [(Integer, Integer)] ->
              Map.Map (Integer, Integer) Integer ->
              (Integer, Integer) ->
              Map.Map (Integer, Integer) Integer
incScore list scoreMap cell =
  let
    owner = calcOwner list cell
    newScoreMap =  case Map.lookup owner scoreMap of
                      Just y -> Map.insert owner (y + 1) scoreMap
                      Nothing -> Map.insert owner 1 scoreMap
  in
    newScoreMap




getGrid ((minX, maxX), (minY, maxY)) =
  [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]



calcOwner :: [(Integer, Integer)] -> (Integer, Integer) -> (Integer, Integer)
calcOwner list cell =
  let
    (winner, _) = foldl (calcOwner_ cell) initialOwner list
  in
    winner



calcOwner_ cell (winning, distance) challenger
    | d == distance = (draw, distance)
    | d < distance  = (challenger, d)
    | otherwise     = (winning, distance)
    where d = dist cell challenger







-- Util
main :: IO ()
main = do
  file <- readFile "input.txt";
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