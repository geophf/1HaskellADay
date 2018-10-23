module Y2018.M10.D24.Exercise where

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
--}

data BFState = BF { ptr :: Int, mem :: [Int] }

-- of course, you can't see the machine's state. That's cheating.

-- given the initial state of the machine to be this:

init :: BFState
init = BF 0 ([72,101,108,108,111,44,32,119,111,114,108,100,33] ++ repeat 0)

-- what does the following bf program do?

type BFProgram = String

bfprogram1 :: BFProgram
bfprogram1 = "[.>]"

-- write a bf program that zeros out an initial state up to the first zero 
-- encountered, ... given that the memory cell under ptr is not zero

bfprogram2 :: BFProgram
bfprogram2 = undefined

-- what is the state of init after running it through bfprogram2?

-- so, of course, we need an interpreter (or a compiler) that runs bf programs:

bfinterpreter :: BFProgram -> BFState -> BFState
bfinterpreter program state0 = undefined

-- Maybe we'll look at factorial tomorrow? Maybe by writing a program that
-- writes BF programs?
