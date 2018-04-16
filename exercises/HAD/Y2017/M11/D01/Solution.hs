{-# LANGUAGE TupleSections, ViewPatterns #-}

module Y2017.M11.D01.Solution where

{--
So, yesterday, we read in source NYT article archives and scanned them. We've 
saved the special characters, and then, manually, we've provided the
replacement ASCII equivalents, where applicable.

Now what?

First, let's read in that config file into a memoizing table. Why? We will see.
--}

import Codec.Compression.GZip
import Control.Arrow ((&&&))
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- below imports available via 1HaskellADay git repository

import Control.DList (emptyDL)
import Control.Scan.Config
import Data.MemoizingTable (MemoizingTable)
import qualified Data.MemoizingTable as MT
import Store.SQL.Connection (withConnection)

import Y2017.M09.D25.Solution (parseHeader)
import Y2017.M10.D02.Solution  -- for block
-- import Y2017.M10.D13.Exercise (compressedArchives)
-- import Y2017.M10.D13.Solution (transformLoad)
import Y2017.M10.D23.Solution  -- for article
import Y2017.M10.D30.Solution  -- for parsing special chars
import Y2017.M10.D31.Solution (printSpecialCharContext)

specialCharsConfig :: FilePath
specialCharsConfig = "Y2017/M10/D31/spcChars.prop"

data AsciiEquiv = DELETE | REPLACE String
   deriving (Eq, Ord, Show)

{--
So, the mappings for just the special characters (no replacement) is DELETE,
otherwise it's REPLACE with the replacement, e.g.:

Â
Â£ EUR

becomes

Â  -> DELETE
Â£ -> REPLACE "EUR"

How would you go about this translation?
--}

parseAscii :: [String] -> AsciiEquiv
parseAscii [] = DELETE
parseAscii (str:_) = REPLACE str

type SpecialCharTable = MemoizingTable SpecialChars AsciiEquiv

readSpecialChars :: FilePath -> IO SpecialCharTable
readSpecialChars =
   fmap (MT.init . (,Map.empty) . Map.fromList
       . map ((head &&& parseAscii . tail) . words) . Set.toList)
      . parseConfig

{--
>>> chars <- readSpecialChars "Y2017/M10/D31/spcChars.prop" 
>>> length (fromTable chars)
47
>>> take 3 (Map.toList (fromTable chars))
[("\194",DELETE),("\194\163",REPLACE "EUR"),("\194\173",DELETE)]

Once you've done that, scan the repository here for special characters. Are 
there any diffs? What are they? If there are new special characters you've 
encountered, add them to the memoizing table (hint: triageMT)

n.b.: since we are not 'mapping back' from ASCII to the special characters,
the readIndex table in the MemoizingTable can be ignored.
--}

repository :: FilePath
repository = "Y2017/M11/D01/NYTOnline_08-29-17_09-05-17_pt3a.txt.gz"

scanArchive :: SpecialCharTable -> FilePath -> IO (Either () [Context])
scanArchive (Map.keysSet . MT.fromTable -> chars) =
   fmap ((\blks -> case concatMap (identify' chars) blks of
                [] -> Left ()
                ctx@(_:_) -> Right ctx) . extractBlocks) . BL.readFile

-- which means we need to layer filtering out the special characters that
-- are already in the special char table

identify' :: MonadPlus m => Set SpecialChars -> Block -> m Context
identify' chars blck =
   let blk = block blck
       art@(_:hdr:_) = BL.lines blk
       header = BL.unpack hdr
       artId = fromMaybe 0 (parseHeader art header) in
   case filter (flip Set.notMember chars . fst)
               (extractSpecialChars (words $ BL.unpack blk)) of
      []        -> mzero
      yup@(_:_) -> return (Ctx artId (enmapify yup))

-- Returns Left (), meaning no new special chars in the new archive, or
-- Right set, with the new special chars not found in our config.

{--
>>> found <- scanArchive chars repository 
>>> found
Right [Ctx {ctxid = 206,
            spcCharCtx = fromList [("\195\174",
                   ["or cr\195\168me\194 fra\195\174che 1 tablespoon"])]},...

Now, from the result returned by scanArchive, call a function that either
replaces the special characters in the target archive or adds the new
characters to the config file for manual update.
--}

replaceSpecialChars :: SpecialCharTable -> Block -> Block
replaceSpecialChars chars =
   Block . BL.pack . refineString chars . BL.unpack . block

refineString :: SpecialCharTable -> String -> String
refineString chars str@(_:_) =
   let (pre, post) = break (\c -> ord c > 127) str
       (spc, rest) = charing post emptyDL
       replacement = case Map.lookup spc (MT.fromTable chars) of
               Just DELETE -> ""
               Just (REPLACE str) -> str
               Nothing -> "" -- end of block ffs
                 -- error ("Could not lookup key " ++ show (map ord spc))
   in pre ++ replacement ++ refineString chars rest
refineString _ [] = []

-- for the above, we need to transform a block with special characters to
-- a block with ASCII replacements

{--
>>> blks <- extractBlocks <$> BL.readFile repository
>>> newblks = either (const (map (replaceSpecialChars chars) blks)) (const []) found
>>> length newblks
200
>>> head newblks
... Full text: LONG BEACH, Calif. -If Ed Cunningham ...

UTF8 em-dash is replaced with ASCII en-dash
--}

updateConfig :: SpecialCharTable -> FilePath -> [Context] -> IO ()
updateConfig specialCharTable config = appendFile config . unlines
   . (["","# -- NEW SPECIAL CHARACTERS ---------------------------",""] ++)
   . Set.toList . ctxn2Set

ctxn2Set :: [Context] -> Set String
ctxn2Set = Set.unions . map (Map.keysSet . spcCharCtx)

{--
>>> either (const (print "huh?")) (updateConfig chars "Y2017/M10/D31/spcChars.prop") found

... and the config file is updated with the new special characters and the
contexten as the context of the new special characters (we can print those out)
--}

-- n.b. updateConfig needs to preserve the special character replacements
-- you've previously defined.

-- If you like, you can print out a friendly message saying what you're doing,
-- based on the function called.

{-- BONUS -----------------------------------------------------------------

With the updated config file, you should now be clear to replace the special
characters in the archive. Repeat all the steps above. Does it do a replace
of the special characters in your archive?

Let's find out.

In the updated archive, are there any special characters? List them.

Hint: Y2017.M10.D30.Solution.identify

Also: write an application that does all the above.

main' :: [String] -> IO ()
main' (configfile:archivefiles) =
   readSpecialChars configfile >>= \chars ->
   mapM_ (\file -> putStrLn ("For archive file: " ++ file) >>
                   extractBlocks <$> BL.readFile file      >>= \blks ->
                   scanArchive chars file                  >>= 
                   either (replaceAndLoad chars blks)
                          (updateConfig chars configfile))
         archivefiles
main' [] = putStrLn (unlines ["","replace <config> <archive1> [archive2, ...]",
     "", "\tReplaces the special characters in archive by ASCII equivalents",
     "\tdefined in <config> and uploads the article set to the data store"])

replaceAndLoad :: SpecialCharTable -> [Block] -> () -> IO ()
replaceAndLoad chars blk _ =
   withConnection NYT (flip transformLoad (map (replaceSpecialChars chars) blk))

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

{--
I chose leaving the source articles undisturbed. I transformed them in memory
and uploaded the transformed set to the database.

>>> main' ["Y2017/M10/D31/spcChars.prop",repository]
For archive file: Y2017/M11/D01/NYTOnline_08-29-17_09-05-17_pt3a.txt.gz

And in the database:

$ select count(1) from article;
200

and a scan of the full_text shows the articles special character-free.
--}
