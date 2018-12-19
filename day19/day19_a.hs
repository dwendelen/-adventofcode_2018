import System.IO
import qualified Data.Map.Strict as Map
import Text.Regex.Posix
import Data.Array
import Data.Bits


--fileName = "example.txt"
--ip = 0

fileName = "input.txt"
ip = 3

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

calc instructions =
  let
    program = listArray (0, (length instructions) - 1) instructions
    initialRegs = Map.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0)]
  in
    exec program initialRegs


exec :: Array Int Instruction -> Map.Map Int Int -> Map.Map Int Int
exec instructions regs =
      let
        idx = regs Map.! ip
        (Instruction op a b r) = instructions ! idx
        val = calcResult regs op a b;
        regWithResult = Map.insert r val regs
        nextIp = (regWithResult Map.! ip) + 1
        regsAfterProgressIp = Map.insert ip nextIp regWithResult
      in
        if idx > snd (bounds instructions)
          then regs
          else exec instructions regsAfterProgressIp

calcResult regs op a b =
  case op of
    Addr -> let
        valA = regs Map.! a
        valB = regs Map.! b
      in
        valA + valB
    Addi -> let
        valA = regs Map.! a
      in
        valA + b
    Mulr -> let
        valA = regs Map.! a
        valB = regs Map.! b
      in
        valA * valB
    Muli -> let
        valA = regs Map.! a
      in
        valA * b

    Banr -> let
        valA = regs Map.! a
        valB = regs Map.! b
      in
        valA .&. valB
    Bani -> let
        valA = regs Map.! a
      in
        valA .&. b
    Borr -> let
        valA = regs Map.! a
        valB = regs Map.! b
      in
        valA .|. valB
    Bori -> let
        valA = regs Map.! a
      in
        valA .|. b

    Setr ->
      regs Map.! a
    Seti ->
      a

    Gtir -> let
        valB = regs Map.! b
      in
        if a > valB then 1 else 0
    Gtri -> let
        valA = regs Map.! a
      in
        if valA > b then 1 else 0
    Gtrr -> let
        valA = regs Map.! a
        valB = regs Map.! b
      in
        if valA > valB then 1 else 0
    Eqir -> let
        valB = regs Map.! b
      in
        if a == valB then 1 else 0
    Eqri -> let
        valA = regs Map.! a
      in
        if valA == b then 1 else 0
    Eqrr -> let
        valA = regs Map.! a
        valB = regs Map.! b
      in
        if valA == valB then 1 else 0

-- Util
main :: IO ()
main = do
  code <- readFile fileName;
  let
    codeRows = lines code
    program = map parseByteCode codeRows

    result = calc program
  print result


parseByteCode :: String -> Instruction
parseByteCode string =
  let
    (_,_,_, match) = string =~ "([a-z]+) ([0-9]+) ([0-9]+) ([0-9]+)" :: (String, String, String, [String])
    op = parseOpCode (match!!0)
    a = read (match!!1) :: Int
    b = read (match!!2) :: Int
    r = read (match!!3) :: Int
  in
    Instruction op a b r

parseOpCode "addr" = Addr
parseOpCode "addi" = Addi
parseOpCode "mulr" = Mulr
parseOpCode "muli" = Muli

parseOpCode "banr" = Banr
parseOpCode "bani" = Bani
parseOpCode "borr" = Borr
parseOpCode "bori" = Bori

parseOpCode "setr" = Setr
parseOpCode "seti" = Seti

parseOpCode "gtir" = Gtir
parseOpCode "gtri" = Gtri
parseOpCode "gtrr" = Gtrr
parseOpCode "eqir" = Eqir
parseOpCode "eqri" = Eqri
parseOpCode "eqrr" = Eqrr