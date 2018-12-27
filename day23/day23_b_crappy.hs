import Text.Regex.Posix
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Map.Strict as Map

{-
  The winning area will always contain one of the tips the of ranges
  This tip will represent the area
-}

type Drone = ((Int, Int, Int), Int)
getRadius (_, r) = r

calc drones =
  let
    initialMap = Map.empty
    filledInMap = Prelude.foldl addDroneToMap initialMap drones
    filledAsList = toList filledInMap

    (_, maxVal) = maximumBy (comparing snd) filledAsList
    filteredList = Prelude.filter (\(_, b) -> b == maxVal) filledAsList

    filteredPos = Prelude.map fst filteredList
    dists = Prelude.map (dist (0,0,0)) filteredPos
  in
    minimum dists

addDroneToMap pointMap drone =
  let
    area = getArea drone
  in
    Prelude.foldl addPointToMap pointMap area

addPointToMap pointMap pos =
  case Map.lookup pos pointMap of
    Just a -> insert pos (a + 1) pointMap
    Nothing -> insert pos 1 pointMap

getArea ((x, y, z), r) =
  [(xx, yy, zz) |
      xx <- [x - r.. x + r],
      yy <- [y - r .. y + r],
      zz <- [z - r .. z + r],
      withinRange ((x, y, z), r) ((xx, yy, zz), 0)
  ]

withinRange (pos1, r) (pos2, _) =
  dist pos1 pos2 <= r

dist (x1, y1, z1) (x2, y2, z2) =
  abs(x2 - x1) + abs(y2 - y1) + abs(z2 - z1)

-- Util
main :: IO ()
main = do
  file <- readFile "example_b.txt";
  let
    codes = lines file
    parsed = Prelude.map parse codes
    result = calc parsed
  print result

parse :: String -> Drone
parse string =
  let
    (_,_,_, match) = string =~ "pos=<(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)>, r=([0-9]+)" :: (String, String, String, [String])
    posX = read (match!!0) :: Int
    posY = read (match!!1) :: Int
    posZ = read (match!!2) :: Int
    r = read (match!!3) :: Int
  in
    ((posX, posY, posZ), r)