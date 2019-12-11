module Main where

import System.Environment
import System.IO
import Debug.Trace
import qualified Control.Monad as CM

import Data.List
import Data.List.Split
import qualified Data.IntMap as M

type InsPtr = Int
type Instruction = Int
type Param = Int
type Program = M.IntMap IWord
type RelativeBase = Int
type IWord = Int --IntcodeWord
type Input = [Int]
type Output = [Int]

data Computer = Computer Program InsPtr RelativeBase Input Output State deriving Show
data State = Run | Halt | Input deriving (Show, Eq)
data ParamMode = Position | Immediate deriving (Show, Eq)
data Opcode = Opcode Int deriving (Show, Ord, Eq)

{- UTILITY FUNCTIONS -}
--------------------------------------------------------------------------------
prepare :: String -> [Int]
prepare program_str = map read $ splitOn "," program_str

safe_subscript :: [a] -> Int -> Maybe a
safe_subscript l i
  | i >= length l = Nothing
  | i < 0 = Nothing
  | otherwise = Just $ l !! i

-- no checking to ensure argument makes sense with opcode
get_param_mode :: Instruction -> Int -> ParamMode
get_param_mode inst i
  | mode == Nothing = Position -- doubtless more modes incoming
  | mode == Just 0 = Position
  | mode == Just 1 = Immediate
  | otherwise = error "Bad param mode"
  where plist = drop 2 $ unfoldr get_digit inst
          where get_digit = (\x -> if x <= 0
                                     then Nothing
                                     else Just (x `rem` 10, x `quot` 10))
        mode = safe_subscript plist i

get_opcode_as_int :: Opcode -> Int
get_opcode_as_int (Opcode i)
  | i >= 1 && i <=9 || i==99 = i
  | otherwise = error $ "Tried to decode an invalid opcode " ++ (show i)

get_num_of_params :: Opcode -> Int
get_num_of_params opc
  | op == 1 || op == 2 || op == 7 || op == 8     = 3
  | op == 3                                      = 1
  | op == 4                                      = 1
  | op == 5 || op == 6                           = 2
  | op == 99                                     = 0
  where op = get_opcode_as_int opc

{- OPERATION FUNCTIONS -}
--------------------------------------------------------------------------------
-- these are the functions that actually update the comp

-- handles addition and multiplication
bin_func :: Computer -> [Int] -> Int -> (Int -> Int -> Int) -> Computer
bin_func (Computer prog insptr _relbase _input _output _state ) arg_list
  write_pos func =
  (Computer (M.insert write_pos new_val prog) insptr _relbase _input _output _state)
  where new_val = func (arg_list !! 0) (arg_list !! 1)

-- assumes input isn't empty
get_inp :: Computer -> Int -> Computer
get_inp (Computer prog insptr _relbase input _output _state) write_pos =
  (Computer (new_prog) insptr _relbase (tail input) _output _state)
  where new_prog = M.insert (prog M.! (insptr+1)) (head input) prog

out:: Computer -> [Int] -> Computer
out (Computer _prog _insptr _relbase _input output _state) arg_list
  = (Computer _prog _insptr _relbase _input (head arg_list:output) _state)

jmp :: Computer -> [Int] -> (Int -> Bool) -> Computer
jmp (Computer _prog insptr _relbase _input _output _state) arg_list comp_func =
  (Computer _prog new_insptr _relbase _input _output _state)
  where new_insptr = if comp_func (head arg_list)
                       then (arg_list !! 1)
                       else insptr + 3

cmp :: Computer -> [Int] -> Int -> (Int -> Int -> Bool) -> Computer
cmp (Computer prog _insptr _relbase _input _output _state) arg_list write_pos
  comp_func =
  (Computer new_prog _insptr _relbase _input _output _state)
  where result = if comp_func (head arg_list) (arg_list !! 1) then 1 else 0
        new_prog = M.insert write_pos result prog

halt :: Computer -> Computer
halt (Computer _prog insptr _relbase _input _output state) =
  (Computer _prog insptr _relbase _input _output Halt)

-- move instruction ptr to next instruction
-- jumps and halt handle their own updating
upd_iptr :: Opcode -> Computer -> Computer
upd_iptr opc (Computer _prog insptr _relbase _input _output _state) =
  (Computer _prog new_insptr _relbase _input _output _state)
  where op = get_opcode_as_int opc
        offset = get_num_of_params opc + 1
        new_insptr = if op == 5 || op == 6 || op == 99
                       then insptr
                       else insptr + offset

execute :: Computer -> Instruction -> Opcode -> Computer
execute (Computer prog insptr relbase input output state) instruction opc
  | op == 1 = bin_func comp arg_list write_pos (+)
  | op == 2 = bin_func comp arg_list write_pos (*)
  | op == 3 = get_inp comp write_pos
  | op == 4 = out comp arg_list
  | op == 5 = jmp comp arg_list (/= 0)
  | op == 6 = jmp comp arg_list (== 0)
  | op == 7 = cmp comp arg_list write_pos (<)
  | op == 8 = cmp comp arg_list write_pos (==)
  | op == 99 = halt comp
  where comp = (Computer prog insptr relbase input output state)
        op = get_opcode_as_int opc
        -- NB: this gets values to use (from either mode), not to write to
        arg_list = map get_arg [0..(get_num_of_params opc - 1)]
        -- findWithDefault: unlikely to matter, but just in case something
        -- points beyond the map. (in that case we can be sure the val is not
        -- actually used)
          where get_arg i = if (get_param_mode instruction i) == Immediate
                              then M.findWithDefault 0 (insptr + i + 1) prog
                              else M.findWithDefault 0 (M.findWithDefault 0 (insptr + i + 1) prog) prog
        -- this gets the index to write to
        write_pos = prog M.! (insptr + (get_num_of_params opc))

tick :: Computer -> Computer
tick (Computer prog insptr relbase input output state) =
  upd_iptr opc $ execute comp instruction opc
  where comp = (Computer prog insptr relbase input output state)
        instruction = prog M.! insptr
        opc = Opcode $ instruction `rem` 100

run :: Computer -> Computer
run (Computer prog insptr relbase input output state)
  -- | trace ("pos: " ++ show insptr ++ "\n" ++ "instruction: " ++ show (prog M.! insptr) ++ "\n" ++ show prog) False = undefined
  | state == Halt = (Computer prog insptr relbase input output state)
  | otherwise = run . tick $ comp
  where comp = (Computer prog insptr relbase input output state)
------------------------------------------------------------

main :: IO ()
main = do
  (f:ns) <- getArgs
  inpf <- readFile $ f
  let program = M.fromAscList . zip [0..] . prepare $ inpf
  let input = map read ns
  let output = [] -- NB: we push to the front!
  let computer = Computer program 0 0 input output Run
  -- putStrLn $ show $ run computer
  (Computer prog insptr relbase input output state) <- return $ run computer
  mapM_ print output
