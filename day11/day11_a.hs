import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing)

--serial = 18
--serial = 42
serial = 3999

main =
  print calc

calc =
  let
    coordinates = [(x, y) | x <- [1..300], y<-[1..300]]
    mappedPower = map (\c -> (c, calcPower c)) coordinates
    power = Map.fromList mappedPower

    squareCoordinates = [(x, y) | x <- [1..298], y <- [1..298]]
    squarePowers = map (\c -> (c, calcSquarePower power c)) squareCoordinates

    max = maximumBy (comparing snd) squarePowers
  in
    max

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


calcSquarePower power (x, y) =
  let
    grid =  [(xx, yy) | xx <- [x..x+2], yy <- [y..y+2]]
    powers = map ((Map.!) power) grid
  in
    sum powers
