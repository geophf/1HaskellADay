module Y2017.M12.D04.Exercise where

{--
"Yeah, but does Haskell do anything ... USEFUL?"

Usually, I find, the best response here is face-punching, but that may not be
your cup-o-tea, so let's do something ... 'USEFUL' with Haskell.

Say you have a big directory: gigabytes of data and thousands of files on your
hard drive, and you have that same-...ish directory in cold-store, AND you need
to clean up your hard drive, but, over time, you've added some files, changed
others ... you're a writer, you know, and you change chapters and stores, and
correct that one, ten, one hundred glaring set of errors.

The problem is this: do you merge? How do you merge? Do you have a merge-tool?
Is the merge-tool on your computer good enough to preserve things? How do you
know it is?

So, we build a merge-preserving-...thingie today.
--}

import System.Directory

{--
https://codereview.stackexchange.com/questions/68908/copying-files-in-haskell

There is that --^

What we want to do is larger in scope:
--}

copyDirs :: FilePath -> FilePath -> IO ()
copyDirs srcDir destDir = undefined

-- copies srcDir to destDir
-- duplicate files are not copied, newer files overwrite/overrule older files

{-- BONUS -----------------------------------------------------------------

Create a directory structure that mimics the directory structure being updated.
Instead of overwriting older files (or not copying over older files), copy the
older files to this duplicated directory structure so the user can manually
compare which files weren't transferred or were overwritten.
--}
