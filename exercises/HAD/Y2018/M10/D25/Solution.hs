module Y2018.M10.D25.Solution where

{--
Today's Haskell problem is the same as yesterday's Haskell problem, except that
we are taking on the looping constructs now and building a compiler instead
of an interpreter for our BF* opcodes, which, as you recall are:

.
+
-
<
>
[
]
--}

import Control.Arrow (second)
import Control.Lens (over, element)
import Control.Monad.State

import Data.Char (chr)

-- below imports available via 1HaskellADay git repository

import Control.DList

import Y2018.M10.D24.Solution

type CompiledCode m = BFState -> m BFState

-- the compiler takes a BFProgram and returns a lambda that represent the
-- program-fragment, returning the rest of the program to be compiled

runCode :: BFState -> [CompiledCode IO] -> IO BFState
runCode st [] = return st
runCode st (op:codes) = op st >>= flip runCode codes

compiler :: BFProgram -> [CompiledCode IO]
compiler [] = []
compiler ('[':prog) =
   let (rest, code) = compileBlock prog in
   code : compiler rest
compiler (op:prog) = compileOp op : compiler prog

-- It may be good to break down the problem of the compiler, having it call
-- a helper function, compileOp, for each op-code to compile

compileOp :: BF_OP_CODE -> CompiledCode IO
compileOp '.' bf = putChar (chr (head (drop (ptr bf) (mem bf)))) >> return bf
compileOp '+' (BF ptr mem) = return (BF ptr (over (element ptr) succ8 mem))
compileOp '-' (BF ptr mem) = return (BF ptr (over (element ptr) pred8 mem))
compileOp '<' (BF ptr mem) = return (BF (pred8 ptr) mem)
compileOp '>' (BF ptr mem) = return (BF (succ ptr) mem)

-- we'll never get to compileOp for '[' or ']' because that's handled by
-- the compiler and compileBlock

succ8,pred8 :: Int -> Int
succ8 x = (if x > 254 then id else succ) x
pred8 x = (if x < 1 then id else pred) x

-- Also, how do you manage compiling a block of code inside [ ]-loops?
-- we treat blocks as a 'single' op-code

compileBlock :: BFProgram -> (BFProgram, CompiledCode IO)
compileBlock = second (loop . dlToList) . cb emptyDL

cb :: DList (CompiledCode IO) -> BFProgram -> (BFProgram, DList (CompiledCode IO))
cb code (']':rest) = (rest, code)
cb code ('[':rest) = -- a block within a block
   let (trail, op) = compileBlock rest in
   cb (code <| op) trail
cb code (op:rest)  = cb (code <| compileOp op) rest

at :: Int -> [Int] -> Int
at idx lst = lst !! idx

loop :: [CompiledCode IO] -> CompiledCode IO
loop code bf@(BF ptr mem) =
   if at ptr mem == 0 then return bf else runCode bf code >>= loop code

-- note that compileBlock treats the block of code in the [ ]-loop atomically.

-- So, once you have a BF* program compiled, run it with some initial state

runBFProgram :: Monad m => [CompiledCode m] -> StateT BFState m ()
runBFProgram opcodes = undefined

-- run the below BF programs against start. What do you get?

bfprog1, bfprog2 :: BFProgram
bfprog1 = "[.>]"
bfprog2 = "[[-]>]"

-- WHAT? A LOOP WITHIN A LOOP? OH! THE HORROR!

{--
>>> prg1 = compiler bfprog1
>>> runCode start prg1
Hello, world!
BF { @13, [72,101,108,108,111,44,32,119,111,114] }

>>> prg2 = compiler bfprog2
>>> runCode start prg2
BF { @13, [0,0,0,0,0,0,0,0,0,0] }
--}
