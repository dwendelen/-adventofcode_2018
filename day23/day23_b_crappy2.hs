import Text.Regex.Posix
import Data.List (maximumBy)
import Data.Ord (comparing)

{-
  The winning area will always contain at least one of the intersections
-}

type Drone = ((Int, Int, Int), Int)
getRadius (_, r) = r

calc drones =
  let
    tips = concatMap getRangeTips drones
    withinRange = map (calcDronesInRange drones) tips

    (winningTip, dronesInRange) = maximumBy (comparing (\(_, b) -> length b)) withinRange
    allEquallyClosePoints = filter (\(_, ds) -> length ds == length dronesInRange) withinRange
  in
    if length allEquallyClosePoints /= 1
      then error "We assume there is only one point closest"
      else (winningTip, length dronesInRange, dist winningTip (0,0,0))

getRangeTips ((x, y, z), r) =
  [
   (x - r, y, z),
   (x + r, y, z),
   (x, y - r, z),
   (x, y + r, z),
   (x, y, z - r),
   (x, y, z + r)
  ]

calcDronesInRange :: [Drone] -> (Int, Int, Int) -> ((Int, Int, Int), [Drone])
calcDronesInRange drones pos =
  (pos, filter (\d -> withinRange d pos) drones)

withinRange (pos1, r) pos2 =
  dist pos1 pos2 <= r

dist (x1, y1, z1) (x2, y2, z2) =
  abs(x2 - x1) + abs(y2 - y1) + abs(z2 - z1)

-- Util
main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    codes = lines file
    parsed = map parse codes
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