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
  map trans instructions


transReg 0 = "a"
transReg 1 = "b"
transReg 2 = "c"
transReg 3 = "d"
transReg 4 = "e"
transReg 5 = "f"

trans (Instruction op a b r) =
  case op of
    Addr -> (transReg r) ++ " = " ++
            (transReg a) ++ " + " ++
            (transReg b)
    Addi -> (transReg r) ++ " = " ++
            (transReg a) ++ " + " ++
            (show     b)
    Mulr -> (transReg r) ++ " = " ++
            (transReg a) ++ " * " ++
            (transReg b)
    Muli -> (transReg r) ++ " = " ++
            (transReg a) ++ " * " ++
            (show     b)

    Banr -> (transReg r) ++ " = " ++
            (transReg a) ++ " & " ++
            (transReg b)
    Bani -> (transReg r) ++ " = " ++
            (transReg a) ++ " & " ++
            (show     b)
    Borr -> (transReg r) ++ " = " ++
            (transReg a) ++ " | " ++
            (transReg b)
    Bori -> (transReg r) ++ " = " ++
            (transReg a) ++ " | " ++
            (show     b)

    Setr -> (transReg r) ++ " = " ++ (transReg a)
    Seti -> (transReg r) ++ " = " ++ (show a)

    Gtir -> (transReg r) ++ " = " ++
            (show     a) ++ " > " ++
            (transReg b)
    Gtri -> (transReg r) ++ " = " ++
            (transReg     a) ++ " > " ++
            (show b)
    Gtrr -> (transReg r) ++ " = " ++
            (transReg     a) ++ " > " ++
            (transReg b)
    Eqir -> (transReg r) ++ " = " ++
            (show     a) ++ " == " ++
            (transReg b)
    Eqri -> (transReg r) ++ " = " ++
            (transReg     a) ++ " == " ++
            (show b)
    Eqrr -> (transReg r) ++ " = " ++
            (transReg     a) ++ " == " ++
            (transReg b)

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