module Y2017.M10.D31.Exercise where

{--
So, yesterday, we catalogued the special characters that occurred in a set of
documents.

Okay, great!

Now, let's capture those in our gitignore-style config file for today's Haskell
exercise, then, ...
 
WARNING! MANUAL LABOUR PART!

...in the config file match the special characters you find to ASCII-equivalents
--}

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Set (Set)

-- below imports available via 1HaskellADay git repository

import Y2017.M10.D02.Exercise
import Y2017.M10.D13.Exercise (compressedArchives)
import Y2017.M10.D30.Exercise

-- step 1: read in the articles from your article archive

-- hint: extractBlocks on archives from Y2017/M10/D13

-- Step 2: extract the special characters and their context-...en.

-- hint: see the work you did yesterday in Y2017.M10.D30.Exercise

-- Step 3: save the special chars out into a gitignore-style config file

-- hint: do you see a pattern for special characters? Do you need to filter
-- out exceptions to those patterns?

saveSpecialChars :: FilePath -> Set SpecialChars -> IO ()
saveSpecialChars file specialCharKeys = undefined

-- Step 4: manually update the config file you've just saved. For each
-- special character combination provide a simplified ASCII string replacement.

{-- e.g.

Your config file may look like this:

# My very own config file of special characters:

\195\161
\195\177
\226\128\148
\226\128\152
\226\128\153
\226\128\153\226\128\157
\226\128\156
\226\128\157
...

Manually update and save the file to look like this:

# My very own config file of special characters:

\195\161 a
\195\177 n
\226\128\148 -
\226\128\152 '
\226\128\153 '
\226\128\153\226\128\157 '"
\226\128\156 "
\226\128\157 "
...

hint: use the context to help you determine what simplified ASCII characters 
are appropriate to replace the special characters.
--}

{-- BONUS -----------------------------------------------------------------

Create an application that does the above, saving the special characters to
the config file named.
--}

main' :: [String] -> IO ()
main' [archiveFile, configFileOut] = undefined  -- do it
main' wrongArgs = undefined -- print out help message

-- Tomorrow we'll read in this config file, then check to see if a new archive
-- file's special characters are fully covered by the config, and, if not,
-- we'll indicate the gaps.
