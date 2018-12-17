import System.IO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Regex.Posix
import Debug.Trace (trace)


data Line = Line LineFormat Int Range
data Range = Range Int Int
data LineFormat = XY | YX

data Type = Clay | Flowing | Still deriving (Eq, Show)

source = (500, 0)

calc lineDescriptions =
  let
    clayCoordinates = concatMap expandLine lineDescriptions

    clayYs = map snd clayCoordinates
    start = minimum clayYs
    end = (maximum clayYs) + 1

    clayWithType = map (\c -> (c, Clay)) clayCoordinates
    blocks = Map.fromList clayWithType

    (resultingBlocks, _) = flowDown end blocks source
    waterBlocksInScope = Map.filterWithKey (\(_, y) typ -> y >= start && y < end && typ == Still) resultingBlocks
  in
    Map.size waterBlocksInScope
    --(Map.size waterBlocksInScope, Map.mapKeys (\(x,y) -> (y, x)) waterBlocksInScope)

flowDown end blocks (x, y)
  | end == y = (blocks, Flowing)
  | isStill blocks (x, y) = (blocks, Still)
  | isClay blocks (x, y) = (blocks, Clay)
  | isFlowing blocks (x, y) = (blocks, Flowing)
  | otherwise =
    let
      (blocks1, res) = flowDown end blocks (x, y + 1)
    in
      if res == Flowing
        then (Map.insert (x, y) Flowing blocks1, Flowing)
        else
          let
            (blocksAfterLR, res2) = flowLeftRight end blocks1 (x, y)
          in
            (Map.insert (x, y) res2 blocksAfterLR, res2)

flowLeftRight end blocks (x, y) =
  let
    (blocks1, left) = flowLeft end blocks (x - 1, y)
    (blocks2, right) = flowRight end blocks1 (x + 1, y)
  in
    if isBlocking left && isBlocking right
      then (blocks2, Still)
      else (turnIntoFlowing blocks2 (x, y), Flowing)

flowLeft end blocks (x, y)
  | isStill blocks (x, y) = (blocks, Still)
  | isClay blocks (x, y) = (blocks, Clay)
  | isFlowing blocks (x, y) = (blocks, Flowing)
  | otherwise =
     let
       (blocks1, res) = flowDown end blocks (x, y + 1)
     in
       if isBlocking res
         then
           let
             (blocksAfterL, res2) = flowLeft end blocks1 (x - 1, y)
           in
             if isBlocking res2
               then (Map.insert (x, y) Still blocksAfterL, Still)
               else (Map.insert (x, y) Flowing blocksAfterL, Flowing)
         else (Map.insert (x, y) Flowing blocks1, Flowing)

flowRight end blocks (x, y)
  | isStill blocks (x, y) = (blocks, Still)
  | isClay blocks (x, y) = (blocks, Clay)
  | isFlowing blocks (x, y) = (blocks, Flowing)
  | otherwise =
     let
       (blocks1, res) = flowDown end blocks (x, y + 1)
     in
       if isBlocking res
         then
           let
             (blocksAfterR, res2) = flowRight end blocks1 (x + 1, y)
           in
             if isBlocking res2
               then (Map.insert (x, y) Still blocksAfterR, Still)
               else (Map.insert (x, y) Flowing blocksAfterR, Flowing)
         else (Map.insert (x, y) Flowing blocks1, Flowing)

turnIntoFlowing blocks (x, y)
  | isFlowing blocks (x, y) = blocks
  | isClay blocks (x, y) = blocks
  | otherwise =
    let
      asFlowing = Map.insert (x, y) Flowing blocks
      asFlowingL = turnIntoFlowing asFlowing (x - 1, y)
      asFlowingLR = turnIntoFlowing asFlowingL (x + 1, y)
    in
      asFlowingLR

isBlocking Still = True
isBlocking Clay = True
isBlocking Flowing = False

isClay blocks xy =
  case (Map.lookup xy blocks) of
    Just Clay -> True
    otherwise -> False

isStill blocks xy =
  case (Map.lookup xy blocks) of
    Just Still -> True
    otherwise -> False

isFlowing blocks xy =
    case (Map.lookup xy blocks) of
      Just Flowing -> True
      otherwise -> False


expandLine (Line XY x (Range yFrom yTo)) =
  [(x, y) | y <- [yFrom..yTo]]
expandLine (Line YX y (Range xFrom xTo)) =
  [(x, y) | x <- [xFrom..xTo]]

-- Util
main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    rows = lines file
    lineDescriptions = map parse rows
    result = calc lineDescriptions
  print result

parse :: String -> Line
parse string =
  let
    (_,_,_, match) = string =~ "([xy])=([0-9]+), [xy]=([0-9]+)\\.\\.([0-9]+)" :: (String, String, String, [String])
    format = if match!!0 == "x" then XY else YX
    a = read (match!!1) :: Int
    b = read (match!!2) :: Int
    c = read (match!!3) :: Int
  in
    Line format a (Range b c)