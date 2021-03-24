module Y2021.M03.D23.Solution where

{-- 
HOWDY! So, what shall we do today?

I would like to implement FORTH, but that's just me. Did you know that in
FORTH pi is simply defined as:

: pi 355 113 */ ;

I LOVE IT! What's even more amazing is that Chuck Moore developed Forth
as a telescope guidance control language, and that definition of pi got the
telescope to point to any star. 355 113 */ was accurate enough for astronomy!

YAY!

Okay, so: let's do this.

The FORTH prompt is ('was'? 'is'?) "OK> " and you enter your FORTH and you
fly.

A number is pushed on the stack and an operator reads from the stack.

Let's implement an RPN with * / + - and . where '.' prints pops the stack and
prints that value.
--}

import Control.Monad.State

type Stack = [Int]

forth :: String -> StateT Stack IO ()
forth str = get >>= lift . doIt (words str) >>= put

doIt :: [String] -> Stack -> IO Stack
doIt [] stk = return stk
doIt (yo:rest) stk = opOrNum yo stk >>= doIt rest

opOrNum :: String -> Stack -> IO Stack
opOrNum cmd stk@(h:t) | cmd == "." = print h >> return t
                      | cmd == "+" = yo (+) h t
                      | cmd == "-" = yo (-) h t
                      | cmd == "/" = yo (div) h t
                      | cmd == "*" = yo (*) h t
                      | otherwise = return (read cmd:stk)
opOrNum num [] = return [read num]

yo :: (Int -> Int -> Int) -> Int -> Stack -> IO Stack
yo op oper (n:ns) = return (op oper n:ns)

repl :: IO ()   -- for "Read Evaluate Print Loop"
repl = repl' []

repl' :: Stack -> IO ()
repl' stk =
   putStr "OK> " >>
   getLine >>= \str ->
   if str == "bye" then putStrLn "Thank you for FORTHing! :)"
   else cont str stk    -- I suppose we could escape with a continuation?

cont :: String -> Stack -> IO ()
cont str stk =
   execStateT (forth str) stk >>=
   repl'

{--
>>> repl
OK> 3 4 + .
7
OK> 12 3 +
OK> 7 + .
22

Let's demonstrate Gauss' sum formula, ... WITH FORTH!

>>> repl
OK> 10 9 8 7 6 5 4 3 2 1
OK> + + + + + + + + + .
55
OK> 2 10 11 * / .
55
OK> bye
Thank you for FORTHing! :)
--}

