import System.IO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)

--input = "51589"
--input = "01245"
--input = "92510"
--input = "59414"

input = "505961"

parsedInput = map (\c -> read [c] :: Int) input
inputLen = length parsedInput

calc =
  let
    m1 = append 3 Map.empty
    m2 = append 7 m1
    result = tick m2 0 1
  in
    result

--tick map idx1 idx2 | trace ("tick: " ++ show map ++ " idx1: " ++ show idx1 ++ " idx2: " ++ show idx2) False = undefined
tick map idx1 idx2 =
  let
    score1 = map Map.! idx1
    score2 = map Map.! idx2
    sum = score1 + score2
    newMap =
      if sum >= 10
        then
          let m1 = append (div sum 10) map
          in append (mod sum 10) m1
        else
          append sum map
    newMapSize = Map.size newMap
    newIdx1 = mod (1 + idx1 + score1) newMapSize
    newIdx2 = mod (1 + idx2 + score2) newMapSize

    matched = tryMatch newMap 0
  in
    case matched of
      Nothing -> tick newMap newIdx1 newIdx2
      Just a -> a

tryMatch mapToMatch 2 = Nothing
tryMatch mapToMatch n
  | Map.size mapToMatch < inputLen + n = Nothing
  | otherwise =
    let
      size = Map.size mapToMatch
      range = [size - inputLen - n .. size - 1 - n]
      vals = map ((Map.!)mapToMatch) range
    in
      if vals == parsedInput
        then Just (size - inputLen - n)
        else tryMatch mapToMatch (n + 1)

append :: a -> Map.Map Int a -> Map.Map Int a
append elem map =
  Map.insert (Map.size map) elem map

-- Util
main :: IO ()
main = do
  print calc