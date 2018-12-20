import Data.Map.Strict as Map
import Data.Tuple

{-
  !!!!!! Solution was right, but I think there is a bug with the walking when stuff comes after the branching
-}
fileName = "input.txt"

data Node = Leaf Direction
          | Concat [Node]
          | Or [Node]
          deriving (Eq, Show)

data Room = Room {
  pos :: (Int, Int),
  doors :: Map Direction Bool
} deriving (Eq, Show)


data Direction = North | East | South | West deriving (Eq, Ord, Show)

moveInDir (x, y) North = (x, y - 1)
moveInDir (x, y) East = (x + 1, y)
moveInDir (x, y) South = (x, y + 1)
moveInDir (x, y) West = (x - 1, y)

inverseDir North = South
inverseDir East = West
inverseDir South = North
inverseDir West = East



calc chars =
  let
    (node, _) = parseConcat chars
    (rooms, pos) = walk initRooms initPos node

    nodes = foldlWithKey (\nodes pos _ -> initNode pos nodes) Map.empty rooms
    dists = updateNode rooms nodes initPos 0

    filtered = Map.filter (\v -> v >= 1000) dists
  in
    size filtered

-- Walking

initPos = (0, 0)
initRooms = insert initPos (initRoom initPos) empty

walk rooms pos (Leaf dir) =
  let
    newRooms = addDoor (pos, dir) rooms
    newPos = moveInDir pos dir
  in
    (newRooms, newPos)

walk rooms pos (Concat nodes) =
  Prelude.foldl (\(rms, ps) node -> walk rms ps node) (rooms, pos) nodes

walk rooms pos (Or nodes) =
  Prelude.foldl (\(rms, _) node -> walk rms pos node) (rooms, pos) nodes


addDoor (pos1, dir1) rooms =
  let
    pos2 = moveInDir pos1 dir1
    dir2 = inverseDir dir1

    (rooms1, room1) = getOrInitRoom pos1 rooms
    (rooms2, room2) = getOrInitRoom pos2 rooms1

    newRoom1 = openDoor_ room1 dir1
    rooms3 = insert pos1 newRoom1 rooms2

    newRoom2 = openDoor_ room2 dir2
    rooms4 = insert pos2 newRoom2 rooms3
  in
    rooms4

openDoor_ (Room { pos = pos, doors = doors }) dir =
  let
    newDoors = insert dir True doors
  in
    Room { pos = pos, doors = newDoors }

getOrInitRoom :: (Int, Int) -> Map (Int, Int) Room -> (Map (Int, Int) Room, Room)
getOrInitRoom pos rooms =
  case Map.lookup pos rooms of
    Nothing -> let
        newRoom = initRoom pos
      in
        (insert pos newRoom rooms, newRoom)
    Just room ->
      (rooms, room)

initRoom pos =
  Room {
    pos = pos,
    doors = fromList [
      (North, False),
      (East, False),
      (South, False),
      (West, False)
    ]
  }

-- Path calculations
initNode pos nodes =
  insert pos (maxBound::Int) nodes

updateNode :: Map (Int, Int) Room -> Map (Int, Int) Int -> (Int, Int) -> Int -> Map (Int, Int) Int
updateNode rooms nodes pos val
  | notMember pos nodes = nodes
  | otherwise =
      let
        oldVal = nodes ! pos
        nodes1 = insert pos val nodes
      in
        if val >= oldVal
          then nodes
          else
            Prelude.foldl (updateNode_ rooms pos val) nodes1 [North, East, South, West]

updateNode_ rooms pos val nodes dir =
    let
        open = isOpen rooms pos dir
        newPos = moveInDir pos dir
        newVal = val + 1
      in
        if open
          then updateNode rooms nodes newPos newVal
          else nodes

isOpen rooms pos dir =
  let
    Room {doors = doors} = rooms ! pos
  in
    doors ! dir

-- Parsing

parseConcat :: [Char] -> (Node, [Char])
parseConcat chars = parseConcat_ chars []

parseConcat_ :: [Char] -> [Node] ->(Node, [Char])
parseConcat_ [] acc =
  (Concat acc, [])
parseConcat_ ('N': rest) acc =
  parseConcat_ rest (acc ++ [Leaf North])
parseConcat_ ('E': rest) acc =
  parseConcat_ rest (acc ++ [Leaf East])
parseConcat_ ('S': rest) acc =
  parseConcat_ rest (acc ++ [Leaf South])
parseConcat_ ('W': rest) acc =
  parseConcat_ rest (acc ++ [Leaf West])
parseConcat_ ('|': rest) acc =
  (Concat acc, '|' : rest)
parseConcat_ (')': rest) acc =
  (Concat acc, ')' : rest)
parseConcat_ ('(': rest) acc =
  let
    (orNode, restrest) = parseOr ('(': rest)
  in
    parseConcat_ restrest (acc ++ [orNode])

parseOr :: [Char] -> (Node, [Char])
parseOr ('(': rest) = parseOr_ rest []

parseOr_ :: [Char] -> [Node] -> (Node, [Char])
parseOr_ chars acc =
  let
    (concatNode, restrest) = parseConcat chars
    newAcc = acc ++ [concatNode]
  in
    case restrest of
      ('|' : restrestrest) -> parseOr_ restrestrest newAcc
      (')' : restrestrest) -> (Or newAcc, restrestrest)

-- Util

main :: IO ()
main = do
  code <- readFile fileName;
  let
    result = calc code
  print result
