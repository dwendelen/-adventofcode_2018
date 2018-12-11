import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Array as Array
import Debug.Trace (trace)

--serial = 18
--serial = 42
serial = 3999

main =
  print calc

size = 300

calc =
  let
    power = calcPowerArray
  in
    loopX power 1 (0,0,0,0)

calcPowerArray:: Array.Array Int (Array.Array Int Int)
calcPowerArray =
  let
    col = map (\x -> (x, calcPowerCol x)) [1..size]
  in
    Array.array (1, size) col

calcPowerCol x =
  let
    col = map (\y -> (y, calcPower (x, y))) [1..size]
  in
    Array.array (1, size) col


loopX power x acc | trace ("New X " ++ show x ++ " acc " ++ show acc) False = undefined
loopX power x acc
  | x > size = acc
  | otherwise =
      let
        newAcc = loopY power x 1 acc
      in
        loopX power (x + 1) newAcc

loopY power x y acc
  | y > size = acc
  | otherwise =
      let
        newAcc = loopZ power x y 1 0 acc
      in
        loopY power x (y + 1) newAcc

loopZ power x y z powerAcc scoreAcc
  | x + z > size || y + z > size = scoreAcc
  | otherwise =
      let
        xz = x + z - 1
        yz = y + z - 1
        rightColumn = [(xz, yy) | yy <- [y..yz - 1]]
        bottomRow = [(xx, yz) | xx <- [x..xz - 1]]
        bottomRightCell = (xz, yz)

        newCells = bottomRightCell : (rightColumn ++ bottomRow)
        newPowers = map (\(x, y) -> (power Array.! x) Array.! y) newCells
        newPower = sum newPowers
        newPowerAcc = powerAcc + newPower

        (mx, my, mz, mp) = scoreAcc
        newScoreAcc = if newPowerAcc > mp then (x, y, z, newPowerAcc) else scoreAcc
      in
        loopZ power x y (z + 1) newPowerAcc newScoreAcc


calcPower (x, y) =
  let
    rId = x + 10
    p1 = rId * y
    p2 = p1 + serial
    p3 = p2 * rId
    hundredsDigit = getHundredsDigit p3
    result = hundredsDigit - 5
  in
    result

getHundredsDigit i =
    mod (div i 100) 10


