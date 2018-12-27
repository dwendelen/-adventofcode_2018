import Text.Regex.Posix
import Data.List (maximumBy, foldl', sortBy)
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
-- knownMinimum = 890 -- Between 894 and 983
knownMinimum = 0--982 -- Between 982 and 983


type Drone = ((Int64, Int64, Int64), Int64)

type BlockRange = ((Int64, Int64), (Int64, Int64), (Int64, Int64))
type DroneInfo = ([Drone], Int64, Int64)
--type Block = (BlockRange, DroneInfo)
type Block = (BlockRange, Int64)

getPessimistic (_, (_, pes, _)) = pes
getOptimistic (_, (_, _, opt)) = opt
getDrones (_, (ds, _, _)) = ds

isStable (_, (_, p, o)) = p == o
isUnstable (_, (_, p, o)) = p /= o

getRadius (_, r) = r

calc drones =
  let
    a = (-2147483648, 2147483647)
    initialBlock = ((a, a, a), fromIntegral (length drones) :: Int64)
  in
    handle drones (knownMinimum :: Int64, maxBound :: Int64) initialBlock

handle :: [Drone] -> (Int64, Int64) -> Block -> (Int64, Int64)
handle drones (best, acc) block | trace ("handle " ++ show block ++ " " ++ show (best, acc)) False = undefined
handle drones (best, acc) block =
      let
        (((x0, x1), (y0, y1), (z0, z1)), nbInRange) = block

        xm = div (x0 + x1 + 1) 2
        ym = div (y0 + y1 + 1) 2
        zm = div (z0 + z1 + 1) 2

        ranges = [
                ((x0, xm - 1),  (y0, ym - 1), (z0, zm - 1)),
                ((x0, xm - 1),  (y0, ym - 1), (zm, z1)),
                ((x0, xm - 1),  (ym, y1),     (z0, zm - 1)),
                ((x0, xm - 1),  (ym, y1),     (zm, z1)),
                ((xm, x1),      (y0, ym - 1), (z0, zm - 1)),
                ((xm, x1),      (y0, ym - 1), (zm, z1)),
                ((xm, x1),      (ym, y1),     (z0, zm - 1)),
                ((xm, x1),      (ym, y1),     (zm, z1))
              ]
        subBlocks = map (\r -> (r, calcNbInRange drones r)) ranges
        sortedSubBlocks = sortBy (flip (comparing (\(_, r) -> r))) subBlocks
        unitDist = dist (unitLoc block) (0,0,0)

        result
          | nbInRange < best =
              (best, acc)
          | (isUnitBlock block) && nbInRange == best && unitDist < acc =
              (nbInRange, unitDist)
          | (isUnitBlock block) && nbInRange == best && unitDist >= acc =
              (best, acc)
          | (isUnitBlock block) && nbInRange > best =
              (nbInRange, unitDist)
          | otherwise =
              foldl' (handle drones) (best, acc) sortedSubBlocks
      in
        result

isUnitBlock :: Block -> Bool
isUnitBlock (((a, b), _, _), _) = a == b

unitLoc (((x, _), (y, _), (z, _)), _) = (x, y, z)

calcNbInRange :: [Drone] -> BlockRange -> Int64
calcNbInRange drones pos =
  let
    inRange = filter (blockInRange pos) drones
  in
    fromIntegral (length inRange) :: Int64

blockInRange ((x0, x1), (y0, y1), (z0, z1)) drone =
  let
    ((xd, yd, zd), _) = drone

    closeX = closest x0 x1 xd
    closeY = closest y0 y1 yd
    closeZ = closest z0 z1 zd
  in
    withinRange drone (closeX, closeY, closeZ)


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

dist :: (Int64, Int64, Int64) -> (Int64, Int64, Int64) -> Int64
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