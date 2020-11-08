{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D10.Exercise where

{--

Well, hi, there. Nice to see you, again!

So, for the Organization of American States, they don't show, that is to
say, they don't render in WikiText {{flagicon|<Country Name>}}. No, instead,
they render countries as {{Country|<Country Name>}} ... obviously.

Meaning that we have to rewrite, instead of reuse, our previous country parser.

This is a good thing.

Why is this a good thing, you ask?

Why do you ask the hard questions, I ask back.

It's a good thing because various data are attacks, attacks upon you, upon
your programs

(they used to call these things 'programs,' kids, you know?)

... upon your ancestors, upon THE WHOLE FRIKKEN PLANET EARTH!

And if your programs can survive these attacks, then your programs become
more resilient.

Handling various data opens your eyes away from one-way-ism, and can SAVE THE
PLANET FROM ALIEN INVADERS ON INDEPENDENCE DAY!

*wind blows through my Presidential Hair as background lighting illums me in
an heroic pose.

So, let's do this: Organization of American States as wikitext. As before, so
now: I give you what's up to au courant, you give me the currant of the au.
--}

import Y2020.M10.D12.Exercise -- ALL THE WAY BACK THEN for Country
import Y2020.M10.D30.Solution (Alliance(Alliance), AllianceMap, dear, moderns)
import qualified Y2020.M10.D30.Solution as A  -- look for flaggie stuff here
import Y2020.M11.D05.Solution (todoPrep)    -- for the updated alliance-parse
import Y2020.M11.D06.Solution (addEU, euDir, eu)
import Y2020.M11.D09.Solution (seedAllianceMap, unDir, un, addUN)

import qualified Data.Map as Map

oasDir :: FilePath
oasDir = "Y2020/M11/D10/"

oas :: FilePath
oas = "org-american-states.wtxt"

stuphfen :: FilePath -> FilePath -> FilePath -> FilePath -> IO AllianceMap
stuphfen blah blee bleu blau =
   seedAllianceMap blah blee >>= flip addEU bleu >>= flip addUN blau

--- Now you:

parseOAS :: String -> Maybe Country
parseOAS line = undefined

-- parseOAS, given a line, parses `|{{Country|Barbados}} [... and stuff]`

-- which means you can now write:

oasParser :: FilePath -> IO Alliance
oasParser = undefined

addOAS :: AllianceMap -> FilePath -> IO AllianceMap
addOAS am merkanFile = undefined

-- DOIT! TOIT!
