module Main where

import System.Environment
import System.IO
import Debug.Trace
import qualified Control.Monad as CM

import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.IntMap as M

type InsPtr = Int
type Instruction = Int
type Param = Int
type Program = M.IntMap IWord
type RelativeBase = Int
type IWord = Int --IntcodeWord
type Input = [Int]

data Computer = Computer Program InsPtr RelativeBase State deriving Show
data State = Run | Halt | GetIn | PutOut deriving (Show, Eq)
data ParamMode = Position | Immediate | Relative deriving (Eq, Ord, Show)
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
  | mode == Just 2 = Relative
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
  | op == 9                                      = 1
  | op == 99                                     = 0
  where op = get_opcode_as_int opc

{- OPERATION FUNCTIONS -}
--------------------------------------------------------------------------------
-- these are the functions that actually update the comp

-- handles addition and multiplication
bin_func :: Computer -> [Int] -> Int -> (Int -> Int -> Int) -> Computer
bin_func (Computer prog insptr _relbase _state ) arg_list
  write_pos func =
  (Computer (M.insert write_pos new_val prog) insptr _relbase _state)
  where new_val = func (arg_list !! 0) (arg_list !! 1)

jmp :: Computer -> [Int] -> (Int -> Bool) -> Computer
jmp (Computer _prog insptr _relbase _state) arg_list comp_func =
  (Computer _prog new_insptr _relbase _state)
  where new_insptr = if comp_func (head arg_list)
                       then (arg_list !! 1)
                       else insptr + 3

cmp :: Computer -> [Int] -> Int -> (Int -> Int -> Bool) -> Computer
cmp (Computer prog _insptr _relbase _state) arg_list write_pos
  comp_func =
  (Computer new_prog _insptr _relbase _state)
  where result = if comp_func (head arg_list) (arg_list !! 1) then 1 else 0
        new_prog = M.insert write_pos result prog

mod_relbase :: Computer -> [Int] -> Computer
mod_relbase (Computer _prog _insptr relbase state) arg_list =
  (Computer _prog _insptr (relbase + head arg_list) state)

change_state :: Computer -> State -> Computer
change_state (Computer _prog insptr _relbase orig_state) new_state =
  (Computer _prog insptr _relbase new_state)

-- move instruction ptr to next instruction
-- jumps and changes of state (getting input, printing output, and halting)
-- handle their own updating (halt doesn't move the ptr)
upd_iptr :: Opcode -> Computer -> Computer
upd_iptr opc (Computer _prog insptr _relbase _state) =
  (Computer _prog new_insptr _relbase _state)
  where op = get_opcode_as_int opc
        offset = get_num_of_params opc + 1
        new_insptr = if op == 3 || op == 4 || op == 5 || op == 6 || op == 99
                       then insptr
                       else insptr + offset

get_arg :: Instruction -> InsPtr -> RelativeBase -> Program -> Int -> IWord
get_arg instruction insptr relbase prog i
  | mode == Immediate = M.findWithDefault 0 (insptr + i + 1) prog
  | mode == Position = M.findWithDefault 0
                       (M.findWithDefault 0 (insptr + i + 1) prog) prog
  | mode == Relative = M.findWithDefault 0
                       (relbase + (M.findWithDefault 0 (insptr + i + 1) prog)) prog
  | otherwise = error "Bad param mode in function get_arg"
  where mode = get_param_mode instruction i

-- NB: this gets /values/ to use (from either mode), not positions
-- (and is not to be used for writing to)

get_arg_list :: Program -> InsPtr -> RelativeBase -> Instruction -> [IWord]
get_arg_list prog insptr relbase instruction =
  map (get_arg instruction insptr relbase prog) [0..(get_num_of_params opc - 1)]
  where opc = Opcode $ instruction `rem` 100

-- This gets the /index/ to write to, for instructions that require writing to
-- memory
-- This assumes that it is being called for a sensible instruction,
-- i.e., one that writes to the position in its last parameter
get_write_pos :: Instruction -> Opcode -> InsPtr -> RelativeBase -> Program -> Int
get_write_pos instruction opc insptr relbase prog
  | mode == Immediate = error "Trying to write to in immediate mode"
  | mode == Position = prog M.! (insptr + num_of_params)
  | mode == Relative = (+) relbase $ prog M.! (insptr + num_of_params)
  | otherwise = error "Trying to write with a bad mode"
    where num_of_params = get_num_of_params opc
          mode = get_param_mode instruction $ num_of_params - 1

execute :: Computer -> Instruction -> Opcode -> Computer
execute (Computer prog insptr relbase state) instruction opc
  | op == 1 = bin_func comp arg_list write_pos (+)
  | op == 2 = bin_func comp arg_list write_pos (*)
  | op == 3 = change_state comp GetIn
  | op == 4 = change_state comp PutOut
  | op == 5 = jmp comp arg_list (/= 0)
  | op == 6 = jmp comp arg_list (== 0)
  | op == 7 = cmp comp arg_list write_pos (<)
  | op == 8 = cmp comp arg_list write_pos (==)
  | op == 9 = mod_relbase comp arg_list
  | op == 99 = change_state comp Halt
  where comp = (Computer prog insptr relbase state)
        op = get_opcode_as_int opc
        --valuse to compute with
        arg_list = get_arg_list prog insptr relbase instruction
        --index to write to
        write_pos = get_write_pos instruction opc insptr relbase prog

tick :: Computer -> Computer
tick (Computer prog insptr relbase state) =
  upd_iptr opc $ execute comp instruction opc
  where comp = (Computer prog insptr relbase state)
        instruction = prog M.! insptr
        opc = Opcode $ instruction `rem` 100

---------------------------------------------------------------

get_new_input :: IO Int
get_new_input = do
  putStrLn "Waiting for input... "
  toret <- getLine
  case (readMaybe toret::Maybe Int) of
    Just x -> do
      return x
    Nothing -> do
      x <- get_new_input
      return x

-- TODO :: deduplicate here
act_on_inp :: Computer -> Int -> Computer
act_on_inp (Computer prog insptr relbase GetIn) inp =
  (Computer new_prog (insptr + (get_num_of_params (Opcode 3)) + 1) relbase Run)
  where instruction = prog M.! insptr
        opc = Opcode $ instruction `rem` 100
        write_pos = get_write_pos instruction opc insptr relbase prog
        new_prog = M.insert write_pos inp prog

print_out :: Program -> InsPtr -> RelativeBase -> IO ()
print_out prog insptr relbase =
  do
    let instruction = prog M.! insptr
    print . head $ get_arg_list prog insptr relbase instruction

-- use this function to debug
run :: Input -> Computer -> IO Computer
run input comp =
  run' input comp
  -- run' input $ traceShow comp comp

run' :: Input -> Computer -> IO Computer
run' input (Computer prog insptr relbase Halt) = do
  return (Computer prog insptr relbase Halt)
run' [] (Computer prog insptr relbase GetIn) = do
  new_int <- get_new_input
  run (new_int:[]) (Computer prog insptr relbase GetIn)
run' (inp:inps) (Computer prog insptr relbase GetIn) = do
  run inps $ act_on_inp (Computer prog insptr relbase GetIn) inp
  where new_prog = M.insert (prog M.! (insptr+1)) inp prog
run' input (Computer prog insptr relbase PutOut) = do
  print_out prog insptr relbase
  run input $ tick (Computer prog (insptr + (get_num_of_params (Opcode 4)) + 1)
                             relbase Run)
run' input comp = run input $ tick comp

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (f:ns) <- getArgs
  program <- fmap (M.fromAscList . zip [0..] . prepare) $ readFile f
  run (map read ns) (Computer program 0 0 Run)
  print "Done"
