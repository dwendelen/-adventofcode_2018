import Text.Regex.Posix
import Data.List (maximumBy)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Data.Int (Int64)
{-
  We will try to find it by splitting the grid
  Every grid has two numbers:
    - Optimistic nb of drones
    - Pessimistic nb of drones

  Every round
    - We look for the highest pessimistic number
    - All grids
        for which the optimistic < the highest pessimistic
        are removed
-}

-- fileName = "example_b.txt"
-- knownMinimum = 4
fileName = "input.txt"
knownMinimum = 982 -- Between 894 and 983


type Drone = ((Int64, Int64, Int64), Int64)

type BlockRange = ((Int64, Int64), (Int64, Int64), (Int64, Int64))
type DroneInfo = ([Drone], Int64, Int64)
type Block = (BlockRange, DroneInfo)

getPessimistic (_, (_, pes, _)) = pes
getOptimistic (_, (_, _, opt)) = opt
getDrones (_, (ds, _, _)) = ds

isStable (_, (_, p, o)) = p == o
isUnstable (_, (_, p, o)) = p /= o

getRadius (_, r) = r

calc drones =
  let
    a = (-2147483648, 2147483647)
    initialBlock = ((a, a, a), (drones, 0, fromIntegral (length drones) :: Int64))
  in
    doRounds [] [initialBlock]

doRounds :: [Block] -> [Block] -> [Block]
doRounds stableBlocks unstableBlocks | trace ("st " ++ show stableBlocks ++ " unst " ++ show (map (\(pos, (_, p, o)) -> (pos, p, o)) unstableBlocks)) False = undefined
doRounds stableBlocks [] = stableBlocks
doRounds stableBlocks unstableBlocks =
  let
    newBlocks0 = concatMap splitBlock unstableBlocks
    newBlocks = concatMap splitBlock newBlocks0

    maxPes0 = maximum (map getPessimistic newBlocks)
    maxPes = if maxPes0 < knownMinimum then knownMinimum else maxPes0

    filteredStables = filter (\b -> getOptimistic b >= maxPes) stableBlocks
    filteredUnstables = filter (\b -> getOptimistic b >= maxPes) newBlocks

    newStables = filteredStables ++ filter isStable filteredUnstables
    newUnstables = filter isUnstable filteredUnstables
  in
    doRounds newStables newUnstables

splitBlock (((x0, x1), (y0, y1), (z0, z1)), (drones, _, _)) =
  let
    xm = div (x0 + x1 + 1) 2
    ym = div (y0 + y1 + 1) 2
    zm = div (z0 + z1 + 1) 2

    ranges = [
        ((x0, xm - 1),  (y0, ym - 1), (z0, ym - 1)),
        ((x0, xm - 1),  (y0, ym - 1), (ym, z1)),
        ((x0, xm - 1),  (ym, y1),     (z0, ym - 1)),
        ((x0, xm - 1),  (ym, y1),     (ym, z1)),
        ((xm, x1),      (y0, ym - 1), (z0, ym - 1)),
        ((xm, x1),      (y0, ym - 1), (ym, z1)),
        ((xm, x1),      (ym, y1),     (z0, ym - 1)),
        ((xm, x1),      (ym, y1),     (ym, z1))
      ]
  in
    map (createBlock drones) ranges

createBlock drones range =
  let
    droneInfo = foldl (updateDroneInfo range) ([], 0, 0) drones
  in
    (range, droneInfo)

updateDroneInfo :: BlockRange -> DroneInfo ->  Drone -> DroneInfo
updateDroneInfo  ((x0, x1), (y0, y1), (z0, z1)) (accD, accP, accO) drone =
  let
    ((xd, yd, zd), _) = drone

    closeX = closest x0 x1 xd
    closeY = closest y0 y1 yd
    closeZ = closest z0 z1 zd
    farX = farest x0 x1 xd
    farY = farest y0 y1 yd
    farZ = farest z0 z1 zd

    opt = withinRange drone (closeX, closeY, closeZ)
    pes = withinRange drone (farX, farY, farZ)
  in
    case (opt, pes) of
      (True, True) -> (drone : accD, accP + 1, accO + 1)
      (True, False) -> (drone : accD, accP, accO + 1)
      (False, _) -> (accD, accP, accO)

closest x0 x1 xd
  | xd <= x0 = x0
  | xd >= x1 = x1
  | otherwise = xd

farest x0 x1 xd
  | xd <= x0 = x1
  | xd >= x1 = x0
  | otherwise =
      let
        d0 = xd - x0
        d1 = x1 - xd
      in
        if d0 > d1
          then x0
          else x1

withinRange (pos1, r) pos2 =
  dist pos1 pos2 <= r

dist (x1, y1, z1) (x2, y2, z2) =
  abs(x2 - x1) + abs(y2 - y1) + abs(z2 - z1)

-- Util
main :: IO ()
main = do
  file <- readFile fileName;
  let
    codes = lines file
    parsed = map parse codes
    result = calc parsed
  print result

parse :: String -> Drone
parse string =
  let
    (_,_,_, match) = string =~ "pos=<(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)>, r=([0-9]+)" :: (String, String, String, [String])
    posX = read (match!!0) :: Int64
    posY = read (match!!1) :: Int64
    posZ = read (match!!2) :: Int64
    r = read (match!!3) :: Int64
  in
    ((posX, posY, posZ), r)