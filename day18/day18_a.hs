import System.IO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Text.Regex.Posix
import Debug.Trace (trace)


data Tile = Open | Trees | Lumberyard deriving (Eq, Show)


calc tiles =
  let
    mapped = zipWith (\y ts -> zipWith (\x t -> ((x, y), t)) [0..] ts) [0..] tiles
    tilesAsMap = Map.fromList (concatMap id mapped)
    afterSimulation = foldl tick tilesAsMap [1..10]

    trees = Map.filter ((==) Trees) afterSimulation
    yards = Map.filter ((==) Lumberyard) afterSimulation

    nbTrees = Map.size trees
    nbYards = Map.size yards
  in
    --afterSimulation
    (nbTrees, nbYards, nbTrees * nbYards)


tick :: Map.Map (Int, Int) Tile -> a -> Map.Map (Int, Int) Tile
tick tiles _ =
  Map.mapWithKey (tickTiles tiles) tiles

tickTiles tiles pos Open =
  let
    nbs = neighbours tiles pos
    trees = filter ((==) Trees) nbs
  in
    if length trees >= 3
      then Trees
      else Open

tickTiles tiles pos Trees =
    let
      nbs = neighbours tiles pos
      lumberyards = filter ((==) Lumberyard) nbs
    in
      if length lumberyards >= 3
        then Lumberyard
        else Trees

tickTiles tiles pos Lumberyard =
  let
    nbs = neighbours tiles pos
    yards = filter ((==) Lumberyard) nbs
    trees = filter ((==) Trees) nbs
  in
    if length yards >= 1 && length trees >= 1
      then Lumberyard
      else Open


neighbours :: Map.Map (Int, Int) Tile -> (Int, Int) -> [Tile]
neighbours tiles (x, y) =
  let
    cells = [(xx, yy) | xx <- [x - 1 .. x + 1], yy <- [y - 1 .. y + 1]]
    otherCells = filter (\c -> c /= (x, y)) cells
  in
    mapMaybe (\c -> Map.lookup c tiles) otherCells


-- Util
main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    rows = lines file
    tiles = map parse rows
    result = calc tiles
  print result

parse :: String -> [Tile]
parse string =
    map parseChar string

parseChar '#' = Lumberyard
parseChar '.' = Open
parseChar '|' = Trees
parseChar _ = error "Unknown char"