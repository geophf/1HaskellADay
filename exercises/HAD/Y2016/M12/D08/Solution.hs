module Y2016.M12.D08.Solution where

import Control.Arrow ((&&&), first)
import Control.Monad
import Data.List (isPrefixOf, transpose)
import Data.Set (Set)
import qualified Data.Set as Set
import Network.HTTP

-- below import available from 1HaskellADay git repository

import Data.Matrix hiding (transpose)

{--
Today's Haskell exercise comes all the way from Smyrna.

Some 2000 or 2500 years ago, somebody scrawled this on a wall in the market:

Μ Η Λ Ο Ν
Η Δ Ο Ν Η
Λ Ο Γ Ο Σ
Ο Ν Ο Μ Α
Ν Η Σ Α Σ

So, I know ΛΟΓΟΣ, is 'logos'/'word,' but what are the other words?

* ΗΔΟΝΗ Pleasure
* ΜΗΛΟΝ Apple
* ΟΝΟΜΑ Name
* ΝΗΣΑΣ Saying (verb, present progressive)

Today's exercise is to construct word squares.

Construct one or more word squares of size n, that is to say, given n, the
length of the word square, choose words from whichever language you like and
construct a word square, that is to say: each word is read horizontally and
vertically.

You can use, e.g. /usr/share/dict/words (on my unix-y laptop, that's where
my dictionary is, YMMV) or you can use these words here:

http://lpaste.net/112882

Or you can use the proteins, starting here:

https://en.wikipedia.org/wiki/List_of_proteins

(ooh, tricky, geophf!)

Or you can use the list of Java keywords here

http://docs.oracle.com/javase/tutorial/java/nutsandbolts/_keywords.html

... okay, don't puke. I'm just saying.

Or whatever you like.
--}

-- well, first up, we need a lexicon. Hm, what's another word for lexicon?
-- I'll look that up in my thesaurus.

type WordPool = Set String

lexicon :: FilePath -> IO WordPool
lexicon = fmap (Set.fromList . words) . readFile

-- *Y2016.M12.D08.Solution> lexicon "/usr/share/dict/words" ~> lexi
-- *Y2016.M12.D08.Solution> length lexi ~> 235886

-- then we need just the words of length n

ners :: Int -> WordPool -> WordPool
ners n = Set.filter ((n ==) . length)

-- *Y2016.M12.D08.Solution> let fivers = ners 5 lexi
-- *Y2016.M12.D08.Solution> length fivers ~> 10230

-- then we pick one of those words to be the first word for the puzzle

setup :: WordPool -> [(String, WordPool)]

{-- urgh.
setup words = case Set.splitRoot words of
   [] -> []
   [a, b, c] -> let newset = Set.union a c in
                (Set.elemAt 0 b, newset):setup newset
I got too clever for myself by half!
--}

setup words = [(a,b) | x <- [0.. pred (Set.size words)],    -- O(n)
                       let a = Set.elemAt x words,   -- O(ln n)
                       let b = Set.delete a words]   -- O(ln n)

-- is there a better way to get the list of pairs of each element of the set
-- and the subset of the set minus that element

{-- 
Now the trick here is that for each following word, it has to form a word
(obviously), but it also has to create pathways for words following it.

So:

Μ Η Λ Ο Ν
Η
Λ
Ο
Ν

When we put the next word, Η Δ Ο Ν Η:

Μ Η Λ Ο Ν
Η Δ Ο Ν Η
Λ Ο
Ο Ν
Ν Η

There must be follow-on words that begin with ΛΟ, ΟΝ, and ΝΗ. If not, we fail 
out and try another word.

Another way to put this is as follows,

Given a list of already placed words, words, produce sets of words for positions
length words + 1 to n such that these sets start with 

drop (length words + 1) (transpose words)

prefices.

Correct? Hm: 'prefices.' That means we need the Data.List.isPrefixOf function.

Well, it sounds good, anyway, let's see if it works!
--}

greek :: [String]
greek = words "ΜΗΛΟΝ ΗΔΟΝΗ ΛΟΓΟΣ ΟΝΟΜΑ ΝΗΣΑΣ"

nextWord :: [String] -> WordPool -> [(String, WordPool)]

{-- 
There's a different, monotomically smaller, word-pool for each word selected.

For example, followOns ["ΜΗΛΟΝ", "ΗΔΟΝΗ"] {"ΛΟΓΟΣ", "ΟΝΟΜΑ", "ΝΗΣΑΣ"} ~>
[("ΛΟΓΟΣ", [{"ΟΝΟΜΑ"}, {"ΝΗΣΑΣ"}])]

I mean, no duh, but it'll be nice to verify that, yes. And then verify that
against the larger case.

Any words selected with zero follow ons in the sets are invalid, except if the 
words selected are the last words, then that's okay ... maybe?

Nah. Too rich for this function, let's just get the next word(s) with their
associated follow-on pools.

--}

nextWord chosenwords pool =
  let raw = setup pool -- setup (Set.filter (startsWith chosenwords) pool)
  in  raw >>= \(word, words) -> 
      let newpool = Set.filter (startsWith (chosenwords ++ [word])) words
      in  guard (not (null newpool)) >> return (word, newpool)

-- um, really??? One of these filters is redundant, I think.

-- removed the first filter, but note the returned value returns a LOCALIZED
-- pool ONLY for the next-next work, NOT to solve the (larger) puzzle.

startsWith :: [String] -> String -> Bool
startsWith = isPrefixOf . startOf

startOf :: [String] -> String
startOf = head . uncurry drop . (length . head &&& id) . transpose

wordSquare :: Int -> [String] -> [Matrix Char]
wordSquare n lexicon = 

-- 1. first we set up our lexicon
   let lexi = ners n (Set.fromList lexicon)

-- this is our basis.

-- 2. then we start the puzzle

       gen0 = setup lexi

-- 3. then we find the next word

   in  gen0                             >>= \(firstW, pool) ->
{--
       let basis = [firstW] in
       nextWord basis pool    >>= \(nextW, wordsForNextW) ->

-- 4. so we have the basis (first word) and the (next-word-after-that-next-word)
-- what we do is automatatize this process n-2 times more

       undefined
--}
       generate (pred n) [firstW] pool >>= \(ans, _) ->
       let mat = fromLists ans in
       guard (cols mat == rows mat) >> return mat

generate :: Int -> [String] -> WordPool -> [([String], WordPool)]
generate 0 a pool = return (a, pool)
generate 1 a pool = fmap (first ((a ++) . pure)) $ setup pool
    -- >>= \(lastW, p) -> return (a ++ [lastW], p)
generate n a pool = nextWord a pool >>= \(nextW, poolForNextW) ->
   setup poolForNextW               >>= \(nextnext, _)         ->
   let trying = [nextW, nextnext] in
   generate (n - 2) (a ++ trying) (foldr Set.delete pool trying)

{-- So:

*Y2016.M12.D08.Solution> pprint . head $ wordSquare 5 (words "MELON EDONE LOGOS ONOMA NESAS") ~>

Matrix 5x5
| 'M' 'E' 'L' 'O' 'N' |
| 'E' 'D' 'O' 'N' 'E' |
| 'L' 'O' 'G' 'O' 'S' |
| 'O' 'N' 'O' 'M' 'A' |
| 'N' 'E' 'S' 'A' 'S' |

(printing Greek characters I found to be troublesome)

Now let's do it in the vernacular:

*Y2016.M12.D08.Solution> lexicon "/usr/share/dict/words" ~> lexit
*Y2016.M12.D08.Solution> lexit >>= pprint . head . wordSquare 5 . Set.toList

*Y2016.M12.D08.Solution> lexit >>= pprint . head . wordSquare 3 . Set.toList
Matrix 3x3
| 'A' 'b' 'e' |
| 'b' 'a' 'a' |
| 'e' 'a' 'n' |

... some of these 'English'-words. I declare. Let's narrow the diversity of the
'words' by using the 2000 common words in the English language.

*Y2016.M12.D08.Solution> simpleHTTP (getRequest "http://lpaste.net/raw/112882") >>= getResponseBody ~> commonWords

*Y2016.M12.D08.Solution> commonWords >>= pprint . head . wordSquare 3 . words
Matrix 3x3
| 'A' 'C' 'T' |
| 'C' 'A' 'R' |
| 'T' 'R' 'Y' |
*Y2016.M12.D08.Solution Network.HTTP> mapM_ putStrLn (rows troisx3)
ACT
CAR
TRY

MUCH better, and:

*Y2016.M12.D08.Solution> commonWords >>= pprint . head . wordSquare 4 . words
Matrix 4x4
| 'A' 'C' 'I' 'D' |
| 'C' 'A' 'R' 'E' |
| 'I' 'R' 'O' 'N' |
| 'D' 'E' 'N' 'Y' |
*Y2016.M12.D08.Solution Network.HTTP> mapM_ putStrLn (rows fourx4)
ACID
CARE
IRON
DENY

*Y2016.M12.D08.Solution> commonWords >>= pprint . head . wordSquare 5 . words
*** Exception: Prelude.head: empty list

... oops, but no fivers from the common words. So it goes.
--}
