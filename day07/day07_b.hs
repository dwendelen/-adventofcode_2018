import System.IO
import Text.Regex.Posix
import qualified Data.Map.Strict as Map
import Data.List (nub, sort, null, delete)
import Data.Char (ord)
import Debug.Trace (trace)

--nbOfWorkers = 2
--fileName = "example.txt"

nbOfWorkers = 5
fileName = "input.txt"

calc :: [(Char, Char)] -> Integer
calc list =
  let
    letters = calcLetters list
    sorted = sort letters
    dependencies = calcDependencies list letters
    result = simulate dependencies sorted
  in
    result

calcLetters list =
  nub ( (map fst list ) ++ (map snd list))

calcDependencies :: [(Char, Char)] -> [Char] -> Map.Map Char [Char]
calcDependencies list letters =
  let
    initial = Map.fromList (map (\l -> (l, [])) letters)
  in
    foldl calcDependencies_ initial list

calcDependencies_ acc (from, to) =
  Map.update (\l -> Just (to : l)) from acc


simulate :: Map.Map Char [Char] -> [Char] -> Integer
simulate dependencies todoSorted  =
  tick dependencies 0 todoSorted []


tick :: Map.Map Char [Char] -> Integer -> [Char] -> [(Char, Integer)] -> Integer
tick _ t notStarted onGoing | trace ("tick t:" ++ show t ++ "; notStarted:" ++ show notStarted ++ "onGoing:" ++ show onGoing) False = undefined
tick _ t [] [] = t
tick dependencies t notStarted onGoing   =
  let
    finished = (filter (\(_, tt) -> tt == t) onGoing)
    stillOnGoing = filter (\e -> not (elem e finished)) onGoing
    stillNotDone = (map fst stillOnGoing) ++ notStarted

    nextToStart =
        if length onGoing >= nbOfWorkers then
          Nothing
        else
          calcNextTaskToStart dependencies notStarted stillNotDone
  in
    if not (null finished)
      then tick dependencies t notStarted stillOnGoing
      else case nextToStart of
          Just charToStart ->
            let
              newTask = (charToStart, t + (calcDuration charToStart))
              newOnGoing = newTask : stillOnGoing
              newNotStarted = delete charToStart notStarted
            in
              tick dependencies t newNotStarted newOnGoing
          Nothing ->
            tick dependencies (t + 1) notStarted stillOnGoing

calcNextTaskToStart dependencies notStarted stillNotDone =
  let
    eligible = filter (dependenciesAreDone dependencies stillNotDone ) notStarted
  in
    case eligible of
      [] -> Nothing
      (e : _) -> Just e


dependenciesAreDone dependencies notDone letter =
  let
    deps = dependencies Map.! letter
    depsNotDone = filter (\dep -> elem dep notDone) deps
  in
    null depsNotDone

calcDuration :: Char -> Integer
calcDuration char =
  let
    int = (ord char) - (ord 'A') + 61
  in
    toInteger int


-- Util
main :: IO ()
main = do
  file <- readFile fileName;
  let
    codes = lines file
    parsed = map parse codes
    result = calc parsed
  print result

parse :: String -> (Char, Char)
parse string =
  let
    (_,_,_, match) = string =~ "Step ([A-Z]) must be finished before step ([A-Z]) can begin." :: (String, String, String, [String])
    dependency = (match!!0)!!0
    depending = (match!!1)!!0
  in
    (depending, dependency)