import System.IO
import Text.Regex.Posix
import Data.List (sortOn, groupBy, maximumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map


-- Log entry
data LogEntry = LogEntry Timestamp Event deriving (Show)

data Timestamp = Timestamp Date Time deriving (Show, Eq, Ord)
data Date = Date Integer Integer deriving (Show, Eq, Ord)
data Time = Time Integer Integer deriving (Show, Eq, Ord)

data Event = StartShift Integer
           | FallsAsleep
           | WakesUp
           deriving (Show)


getTS :: LogEntry -> Timestamp
getTS (LogEntry ts _) =
  ts

-- Shift
data Shift = Shift {
  gid :: Integer,
  state :: ShiftState,
  cycles :: [SleepCycle]
} deriving (Show)
data ShiftState = Awake
                | Sleeping Integer
                deriving (Show)

data SleepCycle = SleepCycle Integer Integer deriving (Show)

getGid (Shift {gid = gid}) = gid
getCycles (Shift {cycles = cycles}) = cycles
getSleep (SleepCycle a b) = b - a

-- SleepRecord

data SleepRecord = SleepRecord Integer [SleepCycle] deriving (Show)

getTotalSleep (SleepRecord _ list)=
  let
    mapped = map getSleep list
  in
    foldl (+) 0 mapped

getGidOfRecord (SleepRecord gid _) =
  gid

getCyclesOfRecord (SleepRecord _ list) = list

-- Main

main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    asStr = lines file
    parsed = map parse asStr
    result = calc parsed
  print result


calc :: [LogEntry] -> (Integer, Integer, Integer)
calc logs =
  let
    sorted = sortLogs logs
    shifts = extractShifts sorted
    records = extractSleepRecords shifts
    maxRecord = maximumBy (comparing getTotalSleep) records

    gid = getGidOfRecord maxRecord
    minute = getInterestingMinute maxRecord
  in
    (gid, minute, gid * minute)



-- EXTRACTING Shifts

extractShifts (x:xs) =
  extractShifts_ xs (newShift x) []
extractShifts _ = []

extractShifts_ :: [LogEntry] -> Shift -> [Shift] -> [Shift]
extractShifts_ [] shift acc = shift : acc
extractShifts_ (x:xs) shift acc =
  case x of
    LogEntry _ (StartShift gid) -> extractShifts_ xs (newShift x) (shift : acc)
    LogEntry (Timestamp _ (Time _ minute)) FallsAsleep -> extractShifts_ xs (sleep minute shift) acc
    LogEntry (Timestamp _ (Time _ minute)) WakesUp -> extractShifts_ xs (wakeup minute shift) acc


newShift (LogEntry _ (StartShift gid)) =
  Shift {
    gid = gid,
    state = Awake,
    cycles = []
  }
newShift _ = error "Can only create a new shift with a StartShift"

sleep minute (Shift { gid = gid, cycles = cycles }) =
  Shift {
    gid = gid,
    state = Sleeping minute,
    cycles = cycles
  }

wakeup minute (Shift { gid = gid, cycles = cycles, state = Sleeping start}) =
  Shift {
      gid = gid,
      state = Awake,
      cycles = (SleepCycle start minute) : cycles
    }
wakeup _ _ = error "Can not wake up when not asleep"

-- EXTRACTING SleepRecords
extractSleepRecords :: [Shift] -> [SleepRecord]
extractSleepRecords shifts =
  let
    sorted = sortOn getGid shifts
    grouped = groupBy (\a b -> (getGid a) == (getGid b)) sorted
    combined = map combineShifts grouped
  in
    combined

combineShifts list =
  let
    first = head list
    gid = getGid first
    initial = SleepRecord gid []
  in
    foldl combineShifts_ initial list

combineShifts_ (SleepRecord gid cycles) shift =
  SleepRecord gid ((getCycles shift) ++ cycles)

-- GET interesting minute
getInterestingMinute :: SleepRecord -> Integer
getInterestingMinute (SleepRecord _ cycles) =
  let
    allMinutes = concatMap (\(SleepCycle start end) -> [start..(end - 1)]) cycles
    his = histo allMinutes
    (min, cnt) = maximumBy (comparing (\(_, b) -> b)) (Map.toList his)
  in
    min
-- SORTING log entries

sortLogs logs =
  sortOn getTS logs


-- PARSING

regexDate = "1518-([0-9]+)-([0-9]+)"
regexTime = "([0-9]+):([0-9]+)"
regexTimestamp = "\\[" ++ regexDate ++ " " ++ regexTime ++ "]"

regexStartShift = "Guard #([0-9]+) begins shift"
regexFallsAsleep = "falls asleep"
regexWakesUp = "wakes up"

regex = regexTimestamp ++ " (" ++
          regexStartShift ++ "|" ++
          regexFallsAsleep ++ "|" ++
          regexWakesUp ++ ")"

parse :: String -> LogEntry
parse string =
  let
    (_,_,_, match) = string =~ regex :: (String, String, String, [String])
    month = read (match!!0) :: Integer
    day = read (match!!1) :: Integer
    minute = read (match!!2) :: Integer
    second = read (match!!3) :: Integer
    eventStr = match!!4
    guardStr = match!!5

    date = Date month day
    time = Time minute second
    timestamp = Timestamp date time

    event = case (eventStr, guardStr) of
      ("falls asleep", "") -> FallsAsleep
      ("wakes up", "") -> WakesUp
      (_, i) -> StartShift (read i :: Integer)
  in
    LogEntry timestamp event



-- HISTO
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