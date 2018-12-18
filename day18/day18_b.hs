import System.IO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Text.Regex.Posix
import Debug.Trace (trace)



{-
  Step = 1387 - 1359 = 28
  (1 000 000 000 - 1387) mod 28 = 5

  -> Same as (556,314,174584,1364),
  -> 174584

  Pattern detected:
  (556,355,197380,1359),
  (556,355,197380,1387),
  (556,355,197380,1415),
  (556,355,197380,1443),
  (556,355,197380,1471),


  (556,355,197380,1359),
  (556,319,177364,1360),
  (554,321,177834,1361),
  (553,320,176960,1362),
  (555,318,176490,1363),
  (556,314,174584,1364),
  (557,319,177683,1365),
  (556,318,176808,1366),
  (561,315,176715,1367),
  (568,313,177784,1368),
  (576,316,182016,1369),
  (583,313,182479,1370),
  (592,318,188256,1371),
  (596,327,194892,1372),
  (602,332,199864,1373),
  (608,340,206720,1374),
  (615,342,210330,1375),
  (622,341,212102,1376),
  (630,337,212310,1377),
  (637,338,215306,1378),
  (639,340,217260,1379),
  (644,347,223468,1380),
  (647,352,227744,1381),
  (634,357,226338,1382),
  (621,357,221697,1383),
  (605,355,214775,1384),
  (589,350,206150,1385),
  (574,349,200326,1386),

-}


data Tile = Open | Trees | Lumberyard deriving (Eq, Show)


calc tiles =
  let
    mapped = zipWith (\y ts -> zipWith (\x t -> ((x, y), t)) [0..] ts) [0..] tiles
    tilesAsMap = Map.fromList (concatMap id mapped)
    f a n = (calcScore a n : f (tick a 0) (n + 1))
  in
     f tilesAsMap 0

calcScore afterSimulation n =
  let
    trees = Map.filter ((==) Trees) afterSimulation
    yards = Map.filter ((==) Lumberyard) afterSimulation

    nbTrees = Map.size trees
    nbYards = Map.size yards
  in
    (nbTrees, nbYards , nbTrees * nbYards, n)

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