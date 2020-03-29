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

type Ptr = IWord
type Instruction = IWord
type Param = IWord
type Program = M.IntMap IWord
type RelativeBase = Ptr
type IWord = Int -- i.e., IntcodeWord
type Input = [IWord]

data Computer = Computer { c_prog :: Program
                         , c_iptr :: Ptr
                         , c_relbase :: RelativeBase
                         , c_state :: State
                         } deriving (Show)
data State = Run | Halt | GetIn | PutOut deriving (Show, Eq)
data ParamMode = Position | Immediate | Relative deriving (Eq, Ord, Show)
data Opcode = Opcode Int deriving (Show, Ord, Eq)

{- UTILITY FUNCTIONS -}
prepare :: String -> Input
prepare program_str = map read $ splitOn "," program_str

safe_subscript :: [a] -> Int -> Maybe a
safe_subscript l i
  | i >= length l = Nothing
  | i < 0 = Nothing
  | otherwise = Just $ l !! i

-- no checking to ensure argument makes sense with opcode
get_param_mode :: Instruction -> Int -> ParamMode
get_param_mode inst i
  | mode == Nothing = Position
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
  | i >= 1 && i <= 9 || i == 99 = i
  | otherwise = error $ "Tried to decode invalid opcode " ++ (show i)

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
-- these are the functions that actually update the comp

-- handles addition and multiplication
bin_func :: Computer -> [IWord] -> Ptr -> (IWord -> IWord -> IWord) -> Computer
bin_func c arg_list write_pos func =
  Computer { c_prog=M.insert write_pos new_val (c_prog c)
           , c_iptr=c_iptr c
           , c_relbase=c_relbase c
           , c_state=c_state c}
  where new_val = func (arg_list !! 0) (arg_list !! 1)

jmp :: Computer -> [IWord] -> (IWord -> Bool) -> Computer
jmp c arg_list comp_func = Computer { c_prog=c_prog c
                                    , c_iptr=new_insptr
                                    , c_relbase=c_relbase c
                                    , c_state=c_state c}
  where new_insptr = if comp_func (head arg_list)
                       then (arg_list !! 1)
                       else (c_iptr c) + 3

cmp :: Computer -> [IWord] -> Ptr -> (IWord -> IWord -> Bool) -> Computer
cmp c arg_list write_pos comp_func = Computer { c_prog=new_prog
                                              , c_iptr=c_iptr c
                                              , c_relbase=c_relbase c
                                              , c_state=c_state c}
  where result = if comp_func (head arg_list) (arg_list !! 1) then 1 else 0
        new_prog = M.insert write_pos result $ c_prog c

mod_relbase :: Computer -> [IWord] -> Computer
mod_relbase c arg_list = Computer { c_prog=c_prog c
                                  , c_iptr=c_iptr c
                                  , c_relbase=c_relbase c + head arg_list
                                  , c_state=c_state c}

change_state :: Computer -> State -> Computer
change_state c new_state = Computer { c_prog=c_prog c
                                    , c_iptr=c_iptr c
                                    , c_relbase=c_relbase c
                                    , c_state=new_state}

-- move instruction ptr to next instruction
-- jumps and changes of state (getting input, printing output, and halting)
-- handle their own updating (halt doesn't move the ptr)
upd_iptr :: Opcode -> Computer -> Computer
upd_iptr opc c = Computer { c_prog=c_prog c
                          , c_iptr=new_insptr
                          , c_relbase=c_relbase c
                          , c_state=c_state c}
  where op = get_opcode_as_int opc
        offset = get_num_of_params opc + 1
        new_insptr = if op == 3 || op == 4 || op == 5 || op == 6 || op == 99
                       then c_iptr c
                       else c_iptr c + offset

get_arg :: Instruction -> Ptr -> RelativeBase -> Program -> Int -> IWord
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

get_arg_list :: Program -> Ptr -> RelativeBase -> Instruction -> [IWord]
get_arg_list prog insptr relbase instruction =
  map (get_arg instruction insptr relbase prog) [0..(get_num_of_params opc - 1)]
  where opc = Opcode $ instruction `rem` 100

-- This gets the /index/ to write to, for instructions that require writing to
-- memory
-- This assumes that it is being called for a sensible instruction,
-- i.e., one that writes to the position in its last parameter
get_write_pos :: Instruction -> Opcode -> Ptr -> RelativeBase -> Program -> Ptr
get_write_pos instruction opc insptr relbase prog
  | mode == Immediate = error "Trying to write to in immediate mode"
  | mode == Position = prog M.! (insptr + num_of_params)
  | mode == Relative = (+) relbase $ prog M.! (insptr + num_of_params)
  | otherwise = error "Trying to write with a bad mode"
    where num_of_params = get_num_of_params opc
          mode = get_param_mode instruction $ num_of_params - 1

execute :: Computer -> Instruction -> Opcode -> Computer
execute comp instruction opc
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
  where op = get_opcode_as_int opc
        --values to compute with
        arg_list = get_arg_list (c_prog comp) (c_iptr comp) (c_relbase comp)
                   instruction
        --index to write to
        write_pos = get_write_pos instruction opc
                    (c_iptr comp) (c_relbase comp) (c_prog comp)

tick :: Computer -> Computer
tick comp = upd_iptr opc $ execute comp instruction opc
  where instruction = c_prog comp M.! c_iptr comp
        opc = Opcode $ instruction `rem` 100

{- IO -}
get_new_input :: IO IWord
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
act_on_inp :: Computer -> IWord -> Computer
act_on_inp comp inp = Computer { c_prog=new_prog
                               , c_iptr=c_iptr comp +
                                         get_num_of_params (Opcode 3) + 1
                               , c_relbase=c_relbase comp
                               , c_state=Run}
  where instruction = c_prog comp M.! c_iptr comp
        opc = Opcode $ instruction `rem` 100
        write_pos = get_write_pos instruction opc (c_iptr comp) (c_relbase comp)
          (c_prog comp)
        new_prog = M.insert write_pos inp (c_prog comp)

print_out :: Program -> Ptr -> RelativeBase -> IO ()
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
  run input (Computer prog (insptr + (get_num_of_params (Opcode 4)) + 1)
                             relbase Run)
run' input comp = run input $ tick comp

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (f:ns) <- getArgs
  program <- fmap (M.fromAscList . zip [0..] . prepare) $ readFile f
  run (map read ns) (Computer program 0 0 Run)
  putStrLn "Done"
