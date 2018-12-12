{-
  Big ass assumption:
    - The simulation converges to the following algorithm
      - All plants move one step to the right
    - This happens within 1000 steps
-}

import System.IO
import Text.Regex.Posix
import qualified Data.Set as Set
import Data.List (head)
import Debug.Trace (trace)

data Rule = Rule Bool Bool Bool Bool Bool deriving (Eq, Ord)

target = 50000000000

calc parsedInitial parsedCodes =
  let
    growRules = Set.fromList (concatMap id parsedCodes)
    endState = step growRules parsedInitial 1000
    scoreAfter1000 = foldl (+) 0 endState
    sizeStable = toInteger(Set.size endState)
  in
    (target - 1000) * sizeStable + scoreAfter1000

step _ currentPlants 0 =
  currentPlants
step rules currentPlants n | trace ("step todo " ++ show n ++ ": " ++ show currentPlants) False = undefined
step rules currentPlants n =
  let
    min = Set.findMin currentPlants
    max = Set.findMax currentPlants
    applied = concatMap (applyRules rules currentPlants) [min - 2..max + 2]
    newPlants = Set.fromList applied
  in
    step rules newPlants (n - 1)

applyRules rules plants idx =
  let
    m2 = Set.member (idx - 2) plants
    m1 = Set.member (idx - 1) plants
    c  = Set.member (idx    ) plants
    p1 = Set.member (idx + 1) plants
    p2 = Set.member (idx + 2) plants
  in
    if Set.member (Rule m2 m1 c p1 p2) rules
      then [idx]
      else []

-- Util
main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    rows = lines file
    initial = head rows
    codes = drop 2 rows
    parsedInitial = parseInitial initial
    parsedCodes = map parseCode codes
    result = calc parsedInitial parsedCodes
  print result

parseInitial :: String -> Set.Set Integer
parseInitial string =
  let
    (_,_,_, match) = string =~ "initial state: ([.#]+)" :: (String, String, String, [String])
    initStr = match!!0
    bools = map ((==) '#') initStr
    indexed = zip [0..] bools
    onlyPlants = filter snd indexed
  in
    Set.fromList (map fst onlyPlants)

parseCode :: String -> [Rule]
parseCode string =
  let
    (_,_,_, match) = string =~ "([.#])([.#])([.#])([.#])([.#]) => ([.#])" :: (String, String, String, [String])
    m2 = match!!0 == "#"
    m1 = match!!1 == "#"
    c  = match!!2 == "#"
    p1 = match!!3 == "#"
    p2 = match!!4 == "#"
    r  = match!!5 == "#"
  in
    if r
      then [Rule m2 m1 c p1 p2]
      else []