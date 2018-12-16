import System.IO
import qualified Data.Map.Strict as Map
import Text.Regex.Posix
import Data.Bits


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

calc examples =
  let
    bools = map tryExample examples
    filtered = filter id bools
  in
    length filtered

tryExample :: Example -> Bool
tryExample example =
  let
    bools = map (tryExampleWithCode example) opCodes
    filtered = filter id bools
  in
    length filtered >= 3

tryExampleWithCode (Example pre (ByteCode op a b r) post) opcode =
  let
    instr = Instruction opcode a b r
    regsIn = listToMap pre
    regsOut = listToMap post
    actual = exec regsIn instr
  in
    actual == Just regsOut

listToMap list =
  Map.fromList (zip [0..] list)

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
  let
    rows = lines file
    examples = parse rows
    result = calc examples
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