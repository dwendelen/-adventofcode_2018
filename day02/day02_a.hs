import System.IO
import Data.List
import qualified Data.Map.Strict as Map


main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    codes = lines file
    result = calc codes
  print result


calc :: [String] -> Integer
calc list =
  let
    (c2, c3) = getCounts list (0, 0)
  in
    c2 * c3


getCounts :: [String] -> (Integer, Integer) -> (Integer, Integer)
getCounts [] acc =
  acc
getCounts (head : tail) (acc2, acc3) =
  let
    hist = histo head
    c2 = if Map.size (Map.filter ((==) 2) hist) > 0 then 1 else 0
    c3 = if Map.size (Map.filter ((==) 3) hist) > 0 then 1 else 0
  in
    getCounts tail (acc2 + c2, acc3 + c3)


histo :: Ord a => [a] -> Map.Map a Integer
histo list =
  histo_ list Map.empty

histo_ :: Ord a => [a] -> Map.Map a Integer -> Map.Map a Integer
histo_ [] acc =
  acc
histo_ (x : xs) acc =
  let
    newAcc = case Map.lookup x acc of
               Just y -> Map.insert x (y + 1) acc
               Nothing -> Map.insert x 1 acc
  in
    histo_ xs newAcc