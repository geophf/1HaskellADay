module Y2018.M10.D25.Exercise where

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

import Control.Monad.State

import Y2018.M10.D24.Exercise

type CompiledCode m = BFState -> m BFState

-- the compiler takes a BFProgram and returns a lambda that represent the
-- program-fragment, returning the rest of the program to be compiled

compiler :: Monad m => BFProgram -> (BFProgram, [CompiledCode m])
compiler programListing = undefined

-- It may be good to break down the problem of the compiler, having it call
-- a helper function, compileOp, for each op-code to compile

type BF_OP_CODE = Char

compileOp :: Monad m => BF_OP_CODE -> CompiledCode m
compileOp opcode = undefined

-- Also, how do you manage compiling a block of code inside [ ]-loops?

compileBlock :: Monad m => BFProgram -> (BFProgram, CompiledCode m)
compileBlock programListing = undefined

-- note that compileBlock treats the block of code in the [ ]-loop atomically.

-- So, once you have a BF* program compiled, run it with some initial state

runBFProgram :: Monad m => [CompiledCode m] -> StateT BFState m ()
runBFProgram opcodes = undefined

-- run the below BF programs against start. What do you get?

bfprog1, bfprog2 :: BFProgram
bfprog1 = "[.>]"
bfprog2 = "[[-]>]"

-- WHAT? A LOOP WITHIN A LOOP? OH! THE HORROR!
