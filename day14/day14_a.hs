import System.IO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)

--input = 5
--input = 18
--input = 2018
input = 505961

calc =
  let
    m1 = append 3 Map.empty
    m2 = append 7 m1
    endMap = tick m2 0 1 (10 + input)
    values = map ((Map.!)endMap) [input..input + 9]
  in
    foldl (\a i -> a ++ show i) "" values

--tick map idx1 idx2 n | trace ("tick " ++ show n ++ ": " ++ show map ++ " idx1: " ++ show idx1 ++ " idx2: " ++ show idx2) False = undefined
tick map idx1 idx2 n
  | Map.size map >= n = map
  | otherwise =
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
    in
      tick newMap newIdx1 newIdx2 n

append :: a -> Map.Map Int a -> Map.Map Int a
append elem map =
  Map.insert (Map.size map) elem map

-- Util
main :: IO ()
main = do
  print calc