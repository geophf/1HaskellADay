module Y2017.M04.D06.Solution where

import Data.List (intercalate)
import System.FilePath ((</>))
import System.Info (os)

{--
So, different operating systems have different ways of addressing stored
information. Windows, we all know, has the CP/M weirdo '\'-character relic that
separates directories from directories from files. Unix/Linux/SCOix/Darwinish
has the '/' for hierarchical naming of inodes, and zOS has '.' to address
boundaries in the mainframe and MacOS 8 has '#' ... just because.

Just because they 'think different' [sic].

And let's not even get started with the FORTH 1024-byte memory blocks, shall we?

So, GIVEN that you know the operating system ... (HOW)? ... create an operator
that returns the appropriate address-separating token to index to resources
managed by the operating system.
--}

type OS = String

getOS :: OS               -- somehow, magically, or see System.Info
getOS = os

separatorF :: OS -> Char
separatorF os | os == "macos"  = '#'
              | os == "zos"    = '.'
              | os == "darwin" = '/'
              | os == "cpm"    = '\\'

-- given an operating system type, return the separator for that OS

-- With that, return the (relative) FilePath for this exercise given various
-- operating systems. The hierarchy to this resource is:

hierarchy :: [String]
hierarchy = words "Y2017 M04 D06 Exercise.hs"

-- so to do the ex's we need the oo-oo-oo-they want any os!

-- geddit?

exAnyOS :: OS -> String
exAnyOS = (`intercalate` hierarchy) . pure . separatorF

{--
>>> exAnyOS "zos"
"Y2017.M04.D06.Exercise.hs"
--}

exZOS, exDarwin, exWindoze, exOldMac :: String
exZOS = exAnyOS "zos"
exDarwin = exAnyOS "darwin"
exWindoze = exAnyOS "cpm"
exOldMac = exAnyOS "macos"

{-- BONUS -----------------------------------------------------------------

Define the path to this exercise using YOUR operating system!
--}

exMyOS :: String
exMyOS = foldr (</>) "" hierarchy

{--
>>> exMyOS 
"Y2017/M04/D06/Exercise.hs"
--}

-- That's a neat-o trick I learned from the quillo.ink Haskell source today
