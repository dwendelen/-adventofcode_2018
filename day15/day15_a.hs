import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.List (sortBy, minimumBy, delete, head, null)
import Data.Ord (comparing)
import Debug.Trace (trace)

data Alliance = Goblin | Elf deriving (Eq, Show)
data Creature = Creature Int Alliance (Int, Int) Int deriving (Eq, Show)
inputFile = "input.txt"

initialHealth = 200
attackPower = 3

isElf (Creature _ Elf _ _) = True
isElf _ = False

isGoblin (Creature _ Goblin _ _) = True
isGoblin _ = False

getId (Creature cId _ _ _) = cId
getPosition (Creature _ _ pos _) = pos
getHealth (Creature _ _ _ health) = health

moveCreature (Creature cId al oldPos health ) newPos | trace ("move " ++ show al ++ " " ++ show oldPos ++ "->" ++ show newPos) False = undefined
moveCreature (Creature cId al _ health ) newPos = Creature cId al newPos health

receiveAttack (Creature cId al pos health ) =
    let
        newHealth = health - attackPower
    in
        if newHealth <= 0
            then Nothing
            else Just (Creature cId al pos newHealth)

areFriendlies (Creature _ a1 _ _) (Creature _ a2 _ _) = a1 == a2
areEnemies c1 c2 = not (areFriendlies c1 c2)



calc openFields creatures =
  let
    emptyPathGrid = pathGridFromList openFields
    (endCreatures, nbFullRounds) = doRounds emptyPathGrid creatures 0
    healths = map getHealth endCreatures
    sumHealth = sum healths
  in
    (nbFullRounds, sumHealth, nbFullRounds * sumHealth)

doRounds emptyPathGrid creatures nbFullRounds | trace ("doRounds " ++ show nbFullRounds ++ " " ++ show (sortBy readingOrderC creatures)) False = undefined
doRounds emptyPathGrid creatures nbFullRounds =
    let
      sortedCreatures = sortBy readingOrderC creatures
      (newCreatures, isDone) = doTurns emptyPathGrid creatures sortedCreatures
    in
      if isDone
        then (newCreatures, nbFullRounds)
        else doRounds emptyPathGrid newCreatures (nbFullRounds + 1)

done creatures =
    let
        allElfs = filter isElf creatures
        allGoblins = filter isGoblin creatures
    in
        null allElfs || null allGoblins

doTurns emptyPathGrid allCreatures [] = (allCreatures, False)
--doTurns emptyPathGrid allCreatures (creature: otherCreatures) | trace ("Turn " ++ show (getPosition creature)) False = undefined
doTurns emptyPathGrid allCreatures (creature: otherCreatures)
  | not (elem (getId creature) (map getId allCreatures)) =
        doTurns emptyPathGrid allCreatures otherCreatures
  | otherwise =
    let
        isDone = done allCreatures
        (creatureAfterMove, allCreaturesAfterMove) = move emptyPathGrid allCreatures creature
        creaturesAfterAttack = attack allCreaturesAfterMove creatureAfterMove
    in
        if isDone
            then
                (allCreatures, True)
            else
                doTurns emptyPathGrid creaturesAfterAttack otherCreatures


move :: PathGrid -> [Creature] -> Creature -> (Creature, [Creature])
move emptyPathGrid allCreatures creature
    | isAdjacentToEnemy allCreatures creature =
          (creature, allCreatures)
    | otherwise =
        let
            newPos = calcNewPos emptyPathGrid allCreatures creature
            otherCreatures = delete creature allCreatures
            newCreature = moveCreature creature newPos
            newCreatures = newCreature : otherCreatures
        in
            (newCreature, newCreatures)

adjacentEnemies allCreatures creature =
    let
        poss = neighbours (getPosition creature)
        creaturesAroundMe = filter (\p -> elem (getPosition p) poss) allCreatures
        enemies = filter (areEnemies creature) creaturesAroundMe
    in
        enemies

isAdjacentToEnemy allCreatures creature =
    let
        enemies = adjacentEnemies allCreatures creature
    in
        length enemies > 0

calcNewPos :: PathGrid -> [Creature] -> Creature -> (Int, Int)
calcNewPos emptyPathGrid allCreatures creature =
    let
        others = delete creature allCreatures
        enemies = filter (areEnemies creature) others
        enemyPositions = map getPosition enemies
        friendlies = filter (areFriendlies creature) others
        friendlyPositions = map getPosition friendlies

        grid1 = foldl addObstacle emptyPathGrid friendlyPositions
        grid2 = foldl addTarget grid1 enemyPositions

        values = map (\p -> (p, getValue grid2 p)) (neighbours (getPosition creature))
        (_, min) = minimumBy (comparing snd) values
        (nextPos, _) = head (filter (\(_, v) -> v == min) values)
    in
        if min == (maxBound::Int)
            then (getPosition creature)
            else nextPos

attack allCreatures creature =
    case (adjacentEnemies allCreatures creature) of
        [] -> allCreatures
        attackCandidates ->
            let
                sorted = sortBy attackOrder attackCandidates
                toAttack = head sorted
                afterAttack = trace ("attack " ++ show (getPosition creature) ++ " -> " ++ show (getPosition toAttack) ) (receiveAttack toAttack)
                withoutAttacked = delete toAttack allCreatures
            in
                case afterAttack of
                    Nothing -> withoutAttacked
                    Just a -> a : withoutAttacked


attackOrder :: Creature -> Creature -> Ordering
attackOrder (Creature _ _ pos1 health1) (Creature _ _ pos2 health2) =
    if health1 == health2
        then readingOrder pos1 pos2
        else compare health1 health2

readingOrderC :: Creature -> Creature -> Ordering
readingOrderC (Creature _ _ pos1 _) (Creature _ _ pos2 _) =
  readingOrder pos1 pos2

readingOrder :: (Int, Int) -> (Int, Int) -> Ordering
readingOrder (x0, y0) (x1, y1) =
  if y0 == y1 then
    compare x0 x1
  else
    compare y0 y1


neighbours (x, y) =
    [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]

----- PathGrid

data PathNode = PathNode Int deriving (Show)
data PathGrid = PathGrid (Map.Map (Int, Int) PathNode) deriving (Show)

pathGridFromList :: [(Int, Int)] -> PathGrid
pathGridFromList list =
  let
    mapped = map (\i -> (i, PathNode (maxBound::Int))) list
  in
    PathGrid (Map.fromList mapped)

addObstacle :: PathGrid -> (Int, Int) -> PathGrid
addObstacle (PathGrid grid) pos =
    PathGrid (Map.delete pos grid)

addTarget :: PathGrid -> (Int, Int) -> PathGrid
addTarget (PathGrid grid) pos =
  let
    newGrid = updateNode pos 0 grid
  in
    PathGrid newGrid

getValue :: PathGrid -> (Int, Int) ->  Int
getValue (PathGrid grid) pos =
    let
        maybeVal = do
            PathNode val <- Map.lookup pos grid;
            Just val
    in
        Maybe.fromMaybe (maxBound::Int) maybeVal

updateNode pos newDist grid
    | not (Map.member pos grid) = grid
    | otherwise =
        let
            PathNode currentDist = grid Map.! pos
        in
            if currentDist <= newDist
                then grid
                else
                    let
                        newNode = PathNode newDist
                        newGrid = Map.insert pos newNode grid
                    in
                        updateNeighbours pos newDist newGrid

updateNeighbours (x, y) dist grid =
  let
    g1 = updateNode (x + 1, y) (dist + 1) grid
    g2 = updateNode (x - 1, y) (dist + 1) g1
    g3 = updateNode (x, y + 1) (dist + 1) g2
    g4 = updateNode (x, y - 1) (dist + 1) g3
  in
    g4




-- Util
main :: IO ()
main = do
  file <- readFile inputFile;
  let
    rows = lines file
    (openFields, creatures) = parse rows
    result = calc openFields creatures
  print result



parse :: [[Char]] -> ([(Int, Int)], [Creature])
parse rows =
  parseRows rows 0 0 ([], [])


parseRows [] _ cId acc = acc
parseRows (r: rs) y cId acc =
  let
    (newAcc, newCId) = parseChars r 0 y cId  acc
  in
    parseRows rs (y + 1) newCId newAcc

parseChars [] _ _ cId acc = (acc, cId)
parseChars (t: ts) x y cId (openFields, creatures) =
  let
    (newAcc, newCId) =
      case t of
        '.' -> (((x,y) : openFields, creatures), cId)
        'G' -> (((x,y) : openFields, (Creature cId Goblin (x,y) initialHealth): creatures), cId + 1)
        'E' -> (((x,y) : openFields, (Creature cId Elf    (x,y) initialHealth): creatures), cId + 1)
        otherwise -> ((openFields, creatures), cId)
  in
    parseChars ts (x + 1) y newCId newAcc


