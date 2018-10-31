module Y2018.M10.D24.Solution where

import Data.Char
import Control.Monad.State

{--
Today, let's go a little Turing Tarpit.

https://esolangs.org/wiki/Brainfuck

Create an interpreter (or compiler, if you prefer) for the following BF
instructions:

>
<
+
-
.
[
]

See the above link for the meaning of these instructions. Also, I'm ignoring
input for now, because I just don't like it. Modify the memory directly instead
of input.

You need a memory pointer, right? Do you need a program counter?

Let's build an interpreter today with the non-looping elements and look
at building a compiler tomorrow, including the looping elements.
--}

data BFState = BF { ptr :: Int, mem :: [Int] }

instance Show BFState where
   show (BF idx mem) = "BF { @" ++ show idx ++ ", " ++ show (take 10 mem) ++ " }"

-- of course, you can't see the machine's state. That's cheating.

-- given the initial state of the machine to be this:

start :: BFState
start = BF 0 ([72,101,108,108,111,44,32,119,111,114,108,100,33,10] ++ repeat 0)

-- what does the following bf program do?

type BFProgram = String

bfprogram1 :: BFProgram
bfprogram1 = ".>.>.>.>.>.>.>.>.>.>.>.>.>"

-- write a bf program that zeros out an initial state up to the first zero 
-- encountered, ... given that the memory cell under ptr is not zero

bfprogram2 :: BFProgram
bfprogram2 = "----------"

-- Of course, the loop falls through if the pointer is at a 0-cell, anyway.

ten :: BFState
ten = BF 0 (10:repeat 0)

-- what is the state of ten after running it through bfprogram2?

-- so, of course, we need an interpreter (or a compiler) that runs bf programs:

bfinterpreter :: BFProgram -> StateT BFState IO ()
bfinterpreter program = mapM_ interpret program >> lift (putStrLn "")

-- Maybe we'll look at factorial tomorrow? Maybe by writing a program that
-- writes BF programs?

type BF_OP_CODE = Char

interpret :: BF_OP_CODE -> StateT BFState IO ()
interpret '.' = get >>= \st@(BF i lst) -> lift (putChar (chr (head (drop i lst))))
interpret '<' = get >>= \ (BF idx lst) -> put $ BF ((if idx == 0 then id else pred) idx) lst
interpret '>' = get >>= \ (BF idx lst) -> put $ BF (succ idx) lst
interpret '+' = get >>= \ (BF idx (h:t)) -> put $ BF idx ((if h == 255 then id else succ) h:t)
interpret '-' = get >>= \(BF idx (h:t)) -> put $ BF idx ((if h ==0 then id else pred) h:t)

{--
>>> runStateT (bfinterpreter bfprogram1) start 
Hello, world!
((),BF { @13, [72,101,108,108,111,44,32,119,111,114] })

>>> runStateT (bfinterpreter bfprogram2) start 

((),BF { @0, [62,101,108,108,111,44,32,119,111,114] })

>>> runStateT (bfinterpreter bfprogram2) ten

((),BF { @0, [0,0,0,0,0,0,0,0,0,0] })
--}
