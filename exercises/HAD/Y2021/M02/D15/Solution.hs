{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D15.Solution where

{--
Look at this directory structure!

wine-reviews-aa-aa-aa-aa
wine-reviews-aa-aa-aa-ab-aa-aa
wine-reviews-aa-aa-aa-ab-aa-ab-aa-aa
... 60 more files ...
wine-reviews-ai-ab-ab
wine-reviews-aj
wine-reviews-ak

This is me, trying to hand-parse-out where embedded newlines broke a JSON
file of wine-reviews.

There has to be a better way than this slog!

Today's Haskell problem: ingest a biggish JSON file (~800k lines) then
show line numbers and lines where embedded newlines break the file.
--}

import Data.List (isPrefixOf)
import Data.Monoid ((<>))

import qualified Y2021.M02.D03.Solution as WR

type LineNumber = Integer
type Line = String

badLine :: Line -> Bool
badLine = (&&) . (/= ',') . last <*> isPrefixOf "\"review\"" . trim

illegalJSON :: FilePath -> IO [(LineNumber, Line)]
illegalJSON file = filter (badLine . snd) . zip [1..] . lines <$> readFile file 

trim :: String -> String
trim [] = []
trim l@(c:r) = if c == ' ' then trim r else l

{--
>>> illegalJSON WR.sampJSONYuge 
[(167991,"    \"review\": \"Brisk and clean, this dry white is the "),
 (712887,"    \"review\": \"This offers generous tones of cherry, dried " ++
         "raspberry, moist tobacco, cured meat and white truffle. It is ")]

But seriously, wine tasting like "moist tobacco and cured meat"?

You have to consider your life-choices if you think wine tastes like that, smh.

"Yeah, dis wine tastes like 'moist tobacco'!"

"Herbert! How do you know that? I told you to quit eating used chew!"

Ewwwwwww!

--}

{-- BONUS ------------------------------------------------------------------

Now that you've identified the problem areas, ... fix the file.
--}

thisDir :: FilePath
thisDir = "Y2021/M02/D15/"

repairJSON :: FilePath -> FilePath -> IO ()
repairJSON brokenFileIn fixedFileOut =
   readFile brokenFileIn >>=
   writeFile fixedFileOut . unlines . scanner badLine . lines

scanner :: Monoid a => (a -> Bool) -> [a] -> [a]
scanner _ [] = []
scanner f l@(_:_) = let (a, as) = scan f l in
   a:scanner f as

scan :: Monoid a => (a -> Bool) -> [a] -> (a, [a])
scan _ [a] = (a, [])
scan f (l:ls) = s' (f l) l ls

s' :: Monoid a => Bool -> a -> [a] -> (a, [a])
s' True x (l:ls) = (x <> l, ls)
s' False x ls =    (x, ls)

fixedFile :: FilePath
fixedFile = "wine-reviews-fixed.json"

{--
>>> repairJSON WR.sampJSONYuge (thisDir ++ fixedFile)
>>> illegalJSON (thisDir ++ "wine-reviews-fixed.json")
[]

... hmmm, but in all of this, there is still one little needle in the 
haystack that I found by, again, hand-parsing the JSON

"t\Thick ..."

I mean, really? Come on!

Manually fixed that spuriously-placed backslash.

Also, there was one wine that was out of sorts, the Carlou 2009. Fixed that.

Three newlines in an 800k~ line JSON-file to ruin a day. Sheesh!
--}
