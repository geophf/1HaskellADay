module Y2017.M10.D31.Solution where

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
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- below imports available via 1HaskellADay git repository

import Y2017.M10.D02.Solution
import Y2017.M10.D13.Exercise (compressedArchives)
import Y2017.M10.D30.Solution

{--
Step 1: read in the articles from your article archive

>>> blks <- concatMap extractBlocks <$> mapM BL.readFile compressedArchives 

Step 2: extract the special characters and their context-...en.

>>> specs = extractSpecialChars . words $ concatMap (BL.unpack . block) blks 
>>> take 3 specs
[("\226\128\157","thing to do.\226\128\157 Like the"),
 ("\226\128\153","sure \226\128\148 it\226\128\153s a good"),
 ("\226\128\148","then sure \226\128\148 it\226\128\153s a")]
--}

-- Step 3: save the special chars out into a gitignore-style config file

saveSpecialChars :: FilePath -> Set SpecialChars -> IO ()
saveSpecialChars file =
   writeFile file . unlines
         . (["# Enumerated special characters and their replacements",""] ++)
         . Set.toList

{--
>>> saveSpecialChars "Y2017/M10/D31/spcChars.prop" (Set.fromList $ map fst specs)
--}

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

>>> mapspec = enmapify specs
>>> mapM_ (\(k,vs) -> putStrLn k >> mapM_ (putStrLn . ('\t':)) (take 2 vs)) (Map.toList mapspec)
Â
	Zealand Designer: Â Related Slideshows
	Post-Surveillance Age: Â Related Slideshows
Â£
	after spending Â£112m on charitable
	out nearly Â£1.2 billion in
Â­
	think of,ââ LewanÂ­dowski said. As
	job pitted LewandowÂ­ski against a
Â¼
	ribs, about 2Â¼ pounds each
...
--}

{-- BONUS -----------------------------------------------------------------

Create an application that does the above, saving the special characters to
the config file named.
--}

main' :: [String] -> IO ()
main' (configFileOut:archiveFiles) = do
   blks <- concatMap extractBlocks <$> mapM BL.readFile archiveFiles
   let specs = extractSpecialChars (words (concatMap (BL.unpack . block) blks))
   saveSpecialChars configFileOut (Set.fromList $ map fst specs)
   putStrLn ("Wrote special characters to " ++ configFileOut)
   putStrLn "\nContext:\n"
   printSpecialCharContext (Map.toList (enmapify specs))

main' [] =
   putStrLn (unlines ["", "scanspec <configout> <archive> [archive, ...]", "",
      "\tscans <archive> for special characters and writes them out to <configout>"])

printSpecialCharContext :: [(String, [String])] -> IO ()
printSpecialCharContext =
   mapM_ (\(k,vs) -> putStrLn k >> mapM_ (putStrLn . ('\t':)) (take 2 vs))

-- Tomorrow we'll read in this config file, then check to see if a new archive
-- file's special characters are fully covered by the config, and, if not,
-- we'll indicate the gaps.
