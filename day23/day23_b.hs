import Data.Monoid ((<>))
import Text.Regex.Posix
import Data.List (maximumBy, foldl', sortBy)
import Data.Ord (comparing)
import Data.Maybe
import Debug.Trace (trace)
import Data.Int (Int64)
import qualified Data.Set as Set


--fileName = "example_b.txt"
fileName = "input.txt"

type Stack = Set.Set (SortingInfo, Block)

type Drone = ((Int64, Int64, Int64), Int64)

type Block = ((Int64, Int64), (Int64, Int64), (Int64, Int64))
data SortingInfo = SortingInfo Int64 Int64 deriving (Show,  Eq)-- nbDrones, distance
instance Ord SortingInfo where
  compare (SortingInfo a1 b1) (SortingInfo a2 b2)  =
    compare a2 a1 <> compare b1 b2

calc drones =
  let
    a = (-2147483648, 2147483647)
    nbDrones = fromIntegral (length drones) :: Int64
    initialSortInfo = SortingInfo nbDrones 0
    initialBlock = (a, a, a)
    stack = Set.insert (initialSortInfo, initialBlock) Set.empty
  in
    handle drones stack

handle :: [Drone] -> Stack -> (Int64, Int64)
handle drones stack | trace ("handle " ++ show (Set.findMin stack) ++ " stack " ++ show stack) False = undefined
handle drones stack =
      let
        ((SortingInfo i1 i2, block), stackAfterPop) = (Set.deleteFindMin stack)

        ((x0, x1), (y0, y1), (z0, z1)) = block

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
        newStack = foldl' (\stck r -> Set.insert ((calcInfo drones r), r) stck ) stackAfterPop ranges
      in
        if isUnitBlock block
          then (i1, i2)
          else handle drones newStack

calcInfo :: [Drone] -> Block -> SortingInfo
calcInfo drones block =
  let
    nbInRange = calcNbInRange drones block
    blockDist = distToOrigin block
  in
    SortingInfo nbInRange blockDist


isUnitBlock :: Block -> Bool
isUnitBlock ((a, b), _, _) = a == b

calcNbInRange :: [Drone] -> Block -> Int64
calcNbInRange drones pos =
  let
    inRange = filter (blockInRange pos) drones
  in
    fromIntegral (length inRange) :: Int64

distToOrigin ((x0, x1), (y0, y1), (z0, z1)) =
  let
    closeX = closest x0 x1 0
    closeY = closest y0 y1 0
    closeZ = closest z0 z1 0
  in
    dist (closeX, closeY, closeZ) (0, 0, 0)

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