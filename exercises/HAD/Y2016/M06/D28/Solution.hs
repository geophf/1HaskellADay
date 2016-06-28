module Y2016.M06.D28.Solution where

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit

{--
Fortunately, the Glorious Glasgo Haskell Compiler library-set provides this
functionality for us, so we must simply use it.

--}

buhbai :: Bool -> ExitCode
buhbai True = ExitSuccess
buhbai False = ExitFailure 4

main = getArgs >>= doesFileExist . head >>= exitWith . buhbai

-- Schnitzengruben.txt is at this directory and at the URL:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M06/D28/Schnitzengruben.txt

{--
Okay, after some digging:

geophf:HAD geophf$ ghc -main-is Y2016.M06.D28.Solution Y2016/M06/D28/Solution.hs -o filechk
[1 of 1] Compiling Y2016.M06.D28.Solution ( Y2016/M06/D28/Solution.hs, Y2016/M06/D28/Solution.o ) [flags changed]
Linking filechk ...

And then (again, after some digging):

geophf:HAD geophf$ ./filechk Y2016/M06/D28/Schnitzengruben.txt 
geophf:HAD geophf$ echo $?
0
geophf:HAD geophf$ ./filechk Y2016/M06/D28/Shatzie.txt
geophf:HAD geophf$ echo $?
4

TA-DAH!
--}
