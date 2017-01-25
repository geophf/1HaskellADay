module Y2017.M01.D25.Exercise where

import System.Directory

{--
Okay, they say 'a' is the second-most popular 'word' in Haskell.

They. Pfft! Well, where will 'they' be when you're in front of your boss 
explaining this statement? Where's your stats? Where's the proof?

Let's break down the problem and prove, or disprove, it, eh?

(see what I did there, eh? I said 'eh' IMPLYING that I meant to say 'a')

(geddit? GEDDIT?)

*sigh

Today's Haskell problem. For a given directory, dir, enumerate all the Haskell
files in that directory.
--}

type Directory = FilePath

haskellFiles :: Directory -> [FilePath]
haskellFiles dir = undefined

{-- BONUS -----------------------------------------------------------------

For a given directory, dir, enumerate all the Haskell file names for that
directory and all subdirectories of that directory. Make sure you can access
those files, by which I mean: include either the relative path or the full
absolute path to the enumerated files.
--}

haskellFilesR :: Directory -> [FilePath]
haskellFilesR dir = undefined

-- So we've got the haskell file names, tomorrow we'll look at file contents
