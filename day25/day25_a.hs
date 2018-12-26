import Text.Regex.Posix
import qualified Data.Map.Strict as Map
import Data.List (foldl', nub)
import Data.Maybe
import Debug.Trace (trace)

--fileName = "test.txt"
fileName = "input.txt"
--fileName = "example_1.txt"

type CId = Int
type Pos = (Int, Int, Int, Int)
type Index = Map.Map Pos CId
type Constellation = Map.Map CId [Pos]

calc points =
  let
    friends = [(a, b) |
        a <- points,
        b <- points,
        inRange a b
      ]
    mapped = map (\(a, b) -> (a, [b])) friends
    friendsAsMap = Map.fromListWith (++) mapped
    friendMap = Map.mapWithKey (\k v -> filter (\i -> i /= k) v) friendsAsMap

    consts = loop (Map.toList friendMap) (Map.empty, Map.empty)
  in
    Map.size consts

loop :: [(Pos, [Pos])] -> (Index, Constellation) -> Constellation
loop [] (_, acc) = acc
loop ((pos, friends): other) (index, const) =
  let
    consts = mapMaybe (\f -> Map.lookup f index) friends
    uniqueConsts = nub consts
    cId1 = head consts
    allMerged = foldl' (\acc cId2 -> mergeConst acc cId1 cId2) (index, const) uniqueConsts

    newIndexConst =
      if consts == []
        then newConst (index, const) pos
        else addPos allMerged pos cId1
  in
    loop other newIndexConst

inRange a b =
  dist a b <= 3

dist (x1, y1, z1, w1) (x2, y2, z2, w2) =
  abs(x2 - x1) + abs(y2 - y1) + abs(z2 - z1) + abs(w2 - w1)

newConst :: (Index, Constellation) -> Pos -> (Index, Constellation)
newConst (index, const) pos =
  let
    newId = Map.size index
    newConst = Map.insert newId [] const
  in
    addPos (index, newConst) pos newId

addPos :: (Index, Constellation) -> Pos -> CId -> (Index, Constellation)
addPos (index, const) pos cId =
  let
    newIndex = Map.insert pos cId index
    newConst = Map.adjust (\o -> pos : o) cId const
  in
    (newIndex, newConst)

mergeConst :: (Index, Constellation) -> CId -> CId -> (Index, Constellation)
mergeConst (index, const) cId1 cId2
  | cId1 == cId2 = (index, const)
  | otherwise =
      let
        items = const Map.! cId2
        removed = Map.delete cId2 const
        added = foldl' (\acc pos -> addPos acc pos cId1) (index, removed) items
      in
        added

-- Util
main :: IO ()
main = do
  file <- readFile fileName;
  let
    codes = lines file
    parsed = map parse codes
    result = calc parsed
  print result

parse :: String -> (Int, Int, Int, Int)
parse string =
  let
    (_,_,_, match) = string =~ "(-?[0-9]+),(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)" :: (String, String, String, [String])
    x = read (match!!0) :: Int
    y = read (match!!1) :: Int
    z = read (match!!2) :: Int
    w = read (match!!3) :: Int
  in
    (x, y, z, w)