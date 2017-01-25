module Y2017.M01.D25.Solution where

import Control.Monad (filterM, (>=>), join)
import Data.List (isSuffixOf, isPrefixOf)
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

haskellFiles :: Directory -> IO [FilePath]
haskellFiles dir =
   getDirectoryContents dir >>=
   filterM doesFileExist
 . map ((dir ++) . ('/':))
 . filter ((||) . isSuffixOf ".hs" <*> isSuffixOf ".lhs")

{--
*Y2017.M01.D25.Solution> getDirectoryContents "Y2017/M01/D10/"
[".","..","christmas_carol.txt","dickens.jpg","Exercise.hs","Solution.hs"]
*Y2017.M01.D25.Solution> haskellFiles "Y2017/M01/D10/"
["Y2017/M01/D10//Exercise.hs","Y2017/M01/D10//Solution.hs"]

BOOM!
--}

{-- BONUS -----------------------------------------------------------------

For a given directory, dir, enumerate all the Haskell file names for that
directory and all subdirectories of that directory. Make sure you can access
those files, by which I mean: include either the relative path or the full
absolute path to the enumerated files.
--}

haskellFilesR :: Directory -> IO [FilePath]
haskellFilesR dir = 
-- first, let's get all the haskell files in this directory
   haskellFiles dir >>= \eff ->
-- next, let's get all the directories in this directory
   directories dir >>=
-- The answer is the sum of the haskell files here and the recurse chase downs
   (mapM haskellFilesR >=> return . (eff ++) . join)

-- not often you can return from a join. I'M FEELING SO MONAD-Y!

directories :: Directory -> IO [Directory]
directories dir =
   getDirectoryContents dir >>=
-- Now see which files at dir are directories
   filterM doesDirectoryExist . map ((dir ++) . ('/':))
-- we don't want looping, so eliminate the '.' and '..' directories
 . filter (not . isPrefixOf ".")

{--
*Y2017.M01.D25.Solution> directories "Data"
["Data/BlockChain","Data/Graphics","Data/Monetary","Data/Numeral","Data/SAIPE",
 "Data/SymbolTable","Data/Time","Data/Tree"]
*Y2017.M01.D25.Solution> haskellFilesR "Data"
["Data/Bag.hs","Data/BidirectionalMap.hs","Data/Matrix.hs","Data/MultiMap.hs",
 "Data/Percentage.hs","Data/QBit.hs","Data/Qubit.hs","Data/Random.hs",
 "Data/Relation.hs","Data/Snowflake.hs","Data/Stream.hs","Data/SymbolTable.hs",
 "Data/Twitter.hs","Data/Universe.hs","Data/XHTML.hs",
 "Data/BlockChain/Block/BlockInfo.hs","Data/BlockChain/Block/Blocks.hs",
 "Data/BlockChain/Block/BlocksOffline.hs","Data/BlockChain/Block/Graphs.hs",
 "Data/BlockChain/Block/Summary.hs","Data/BlockChain/Block/Transactions.hs",
 "Data/BlockChain/Block/Types.hs","Data/BlockChain/Block/Utils.hs",
 "Data/BlockChain/Block/Web.hs","Data/Graphics/BoundingBox.hs",
 "Data/Graphics/Cell.hs","Data/Graphics/Color.hs","Data/Graphics/SVG.hs",
 "Data/Monetary/BitCoin.hs","Data/Monetary/Currency.hs","Data/Monetary/USD.hs",
 "Data/Numeral/Boolean.hs","Data/SAIPE/USCounties.hs","Data/SAIPE/USStates.hs",
 "Data/SymbolTable/Compiler.hs","Data/SymbolTable/Decompiler.hs",
 "Data/Time/Calendar/Month.hs","Data/Tree/Merkle.hs"]
--}

-- So we've got the haskell file names, tomorrow we'll look at file contents
