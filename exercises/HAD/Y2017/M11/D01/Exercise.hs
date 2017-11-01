module Y2017.M11.D01.Exercise where

{--
So, yesterday, we read in source NYT article archives and scanned them. We've 
saved the special characters, and then, manually, we've provided the
replacement ASCII equivalents, where applicable.

Now what?

First, let's read in that file of special characters into a memoizing table. 
Why? We will see.
--}

import Data.Set (Set)

-- below imports available via 1HaskellADay git repository

import Control.Scan.Config
import Data.MemoizingTable

import Y2017.M10.D02.Exercise  -- for block
import Y2017.M10.D30.Exercise  -- for special char parsing

specialCharsFile :: FilePath
specialCharsFile = "Y2017/M10/D31/spcChars.prop"

data AsciiEquiv = DELETE | REPLACE String
   deriving (Eq, Show)

{--
So, the mappings for just the special characters (no replacement) is DELETE,
otherwise it's REPLACE with the replacement, e.g.:

Â
Â£ EUR

becomes

Â  -> DELETE
Â£ -> REPLACE "EUR"

How would you affect this transition?
--}

parseAscii :: [String] -> AsciiEquiv
parseAscii asciival = undefined

type SpecialCharTable = MemoizingTable SpecialChars AsciiEquiv

readSpecialChars :: FilePath -> IO SpecialCharTable
readSpecialChars config = undefined

{--
Once you've done that, scan the repository here for special characters. Are 
there any diffs? What are they? If there are new special characters you've 
encountered, add them to the memoizing table (hint: triageMT)

n.b.: since we are not 'mapping back' from ASCII to the special characters,
the readIndex table in the MemoizingTable can be ignored.
--}

repository :: FilePath
repository = "Y2017/M11/D01/NYTOnline_08-29-17_09-05-17_pt3a.txt.gz"

scanArchive :: SpecialCharTable -> FilePath -> IO (Either () [Context])
scanArchive archive specialChars = undefined

-- Returns Left (), meaning no new special chars in the new archive, or
-- Right set, with the new special chars not found in our config.

{--
Now, from the result returned by scanArchive, call a function that either
replaces the special characters in the target archive or adds the new
characters to the config file for manual update.
--}

replaceSpecialChars :: SpecialCharTable -> Block -> Block
replaceSpecialChars specialCharTable block = undefined

-- updates block with special chars to block with ASCII-equivalents
-- hint: you need to extract then replace special characters in block.
-- ... now where have we written an extract function. Hm.

updateConfig :: SpecialCharTable -> FilePath -> [Context] -> IO ()
updateConfig specialCharTable config ctxn = undefined

-- n.b. updateConfig needs to preserve the special character replacements
-- you've previously defined. Hint: appendFile?

-- If you like, you can print out a friendly message saying what you're doing,
-- based on the function called.

{-- BONUS -----------------------------------------------------------------

With the updated config file, you should now be clear to replace the special
characters in the archive. Repeat all the steps above. Does it do a replace
of the special characters in your archive?

Let's find out.

In the updated archive, are there any special characters? List them.

Hint: Y2017.M10.D30.Exercise.identify

Also: write an application that does all the above.
--}

main' :: [String] -> IO ()
main' (configfile:archivefiles) = undefined -- do it
main' [] = undefined -- print help message

{--
I'm of two minds.

On the one hand, I could just simply, instead of writing out a new, sanitized,
archive, simply upload the in-memory sanitized version to the database. On the
other hand, I do have physical evidence of the transition: from original
archive to sanitized one. That artifact has worth when things go South and the
transition states are useful for debugging, so ...

You can save out the resulting sanitized version to the file system or you can
upload the revised archive right to the database. Your choice.

Why did you go with the option you chose?
--}
