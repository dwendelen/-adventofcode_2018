import System.IO
import qualified Data.Map.Strict as Map
import Text.Regex.Posix
import Data.Bits
import Data.Maybe
import Data.List

data Example = Example [Int] ByteCode [Int] deriving (Eq, Show)
data ByteCode = ByteCode Int Int Int Int deriving (Eq, Show)

data Instruction = Instruction OpCode Int Int Int deriving (Eq, Show)
data OpCode   = Addr
              | Addi
              | Mulr
              | Muli

              | Banr
              | Bani
              | Borr
              | Bori

              | Setr
              | Seti

              | Gtir
              | Gtri
              | Gtrr
              | Eqir
              | Eqri
              | Eqrr
              deriving (Eq, Show)

opCodes = [ Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori,
            Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

calc examples byteCodes =
  let
    mapped = mapMaybe tryExample examples
    reduced = Map.fromListWith mergeExamples mapped
    resolved = resolve (Map.toList reduced)
    resolvedAsMap = Map.fromList resolved

    instructions = map (mapToInstructions resolvedAsMap) byteCodes
    regs = foldl execOrCrash (Map.fromList [(0,0), (1, 0), (2, 0), (3, 0)]) instructions
  in
    regs Map.! 0

mergeExamples :: [OpCode] -> [OpCode] -> [OpCode]
mergeExamples a b =
  let
    merged = intersect a b
  in
    if merged == []
      then error ("Empty after merging " ++ show a ++ " and " ++ show b)
      else merged

-- Resolve

resolve :: [(Int, [OpCode])] -> [(Int, OpCode)]
resolve examples =
  resolveLoop [] examples

resolveLoop resolved [] = resolved
resolveLoop resolved todo =
  let
    knownOpCodes = map snd resolved
    reducedTodos = map (removeKnowns knownOpCodes) todo
    resolvedTodos = filter (\(_, c) -> length c == 1) reducedTodos
    mappedResolvedTodos = map (\(i, c) -> (i, head c)) resolvedTodos
    unresolvedTodos = filter (\(_, c) -> length c /= 1) reducedTodos
  in
    if mappedResolvedTodos == []
      then error ("No progress with " ++ show resolved ++ " todos: " ++ show todo)
      else resolveLoop (mappedResolvedTodos ++ resolved) unresolvedTodos

removeKnowns knownCodes (i, candidates) =
  let
    filtered = filter (\c -> not (elem c knownCodes)) candidates
  in
    (i, filtered)


mapToInstructions :: Map.Map Int OpCode -> ByteCode -> Instruction
mapToInstructions resolvedAsMap (ByteCode op a b c) =
  let
    opcode = resolvedAsMap Map.! op
  in
    Instruction opcode a b c

-- Trying stuff out

tryExample :: Example -> Maybe (Int, [OpCode])
tryExample example =
  let
    mappings = mapMaybe (tryExampleWithCode example) opCodes
  in
    if mappings == []
      then Nothing
      else Just (fst (head mappings), map snd mappings)


tryExampleWithCode (Example pre (ByteCode op a b r) post) opcode =
  let
    instr = Instruction opcode a b r
    regsIn = listToMap pre
    regsOut = listToMap post
    actual = exec regsIn instr
  in
    if actual == Just regsOut
      then Just (op, opcode)
      else Nothing

listToMap list =
  Map.fromList (zip [0..] list)

-- Executing

execOrCrash :: Map.Map Int Int -> Instruction -> Map.Map Int Int
execOrCrash regs instr =
  fromJust (exec regs instr)

exec :: Map.Map Int Int -> Instruction -> Maybe (Map.Map Int Int)
exec regs (Instruction op a b r) =
  do
    val <- calcResult regs op a b;
    if r > 4 || r < 0
      then Nothing
      else Just (Map.insert r val regs)

calcResult regs op a b =
  case op of
    Addr -> do
      valA <- Map.lookup a regs;
      valB <- Map.lookup b regs;
      Just (valA + valB)
    Addi -> do
      valA <- Map.lookup a regs;
      Just (valA + b)
    Mulr -> do
      valA <- Map.lookup a regs;
      valB <- Map.lookup b regs;
      Just (valA * valB)
    Muli -> do
      valA <- Map.lookup a regs;
      Just (valA * b)

    Banr -> do
      valA <- Map.lookup a regs;
      valB <- Map.lookup b regs;
      Just (valA .&. valB)
    Bani -> do
      valA <- Map.lookup a regs;
      Just (valA .&. b)
    Borr -> do
      valA <- Map.lookup a regs;
      valB <- Map.lookup b regs;
      Just (valA .|. valB)
    Bori -> do
      valA <- Map.lookup a regs;
      Just (valA .|. b)

    Setr ->
      Map.lookup a regs
    Seti ->
      Just a

    Gtir -> do
      valB <- Map.lookup b regs;
      Just (if a > valB then 1 else 0)
    Gtri -> do
      valA <- Map.lookup a regs;
      Just (if valA > b then 1 else 0)
    Gtrr -> do
      valA <- Map.lookup a regs;
      valB <- Map.lookup b regs;
      Just (if valA > valB then 1 else 0)
    Eqir -> do
      valB <- Map.lookup b regs;
      Just (if a == valB then 1 else 0)
    Eqri -> do
      valA <- Map.lookup a regs;
      Just (if valA == b then 1 else 0)
    Eqrr -> do
      valA <- Map.lookup a regs;
      valB <- Map.lookup b regs;
      Just (if valA == valB then 1 else 0)

-- Util
main :: IO ()
main = do
  file <- readFile "examples_input.txt";
  code <- readFile "program_input.txt";
  let
    rows = lines file
    examples = parse rows

    codeRows = lines code
    program = map parseByteCode codeRows

    result = calc examples program
  print result

parse :: [String] -> [Example]
parse rows =
  parse_ rows []

parse_ [] acc = acc
parse_ (l1: l2: l3: _: rest) acc =
  let
    pre = parsePre l1
    inst = parseByteCode l2
    post = parsePost l3
    newAcc = acc ++ [Example pre inst post]
  in
    parse_ rest newAcc
parse_ rest acc = error ("Bad input file, could not match " ++ show rest)


parsePre :: String -> [Int]
parsePre string =
  let
    (_,_,_, match) = string =~ "Before: \\[([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+)]" :: (String, String, String, [String])
    r1 = read (match!!0) :: Int
    r2 = read (match!!1) :: Int
    r3 = read (match!!2) :: Int
    r4 = read (match!!3) :: Int
  in
    [r1, r2, r3, r4]

parseByteCode :: String -> ByteCode
parseByteCode string =
  let
    (_,_,_, match) = string =~ "([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)" :: (String, String, String, [String])
    op = read (match!!0) :: Int
    a = read (match!!1) :: Int
    b = read (match!!2) :: Int
    r = read (match!!3) :: Int
  in
    ByteCode op a b r

parsePost :: String -> [Int]
parsePost string =
  let
    (_,_,_, match) = string =~ "After:  \\[([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+)]" :: (String, String, String, [String])
    r1 = read (match!!0) :: Int
    r2 = read (match!!1) :: Int
    r3 = read (match!!2) :: Int
    r4 = read (match!!3) :: Int
  in
    [r1, r2, r3, r4]

