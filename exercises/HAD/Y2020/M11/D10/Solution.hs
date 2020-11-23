{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D10.Solution where

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

import Y2020.M10.D12.Solution -- ALL THE WAY BACK THEN for Country
import Y2020.M10.D30.Solution (Alliance(Alliance), AllianceMap, dear, moderns)
import qualified Y2020.M10.D30.Solution as A  -- look for flaggie stuff here
import Y2020.M11.D05.Solution (todoPrep)    -- for the updated alliance-parse
import Y2020.M11.D06.Solution (euDir, eu, adder)
import Y2020.M11.D09.Solution (seedAllianceMap, unDir, un, addUN)

import Data.List (stripPrefix)

import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import qualified Data.Set as Set

import Data.Text (pack)

oasDir :: FilePath
oasDir = "Y2020/M11/D10/"

oas :: FilePath
oas = "org-american-states.wtxt"

stuphfen :: FilePath -> FilePath -> FilePath -> IO AllianceMap
stuphfen blah blee blau =
   seedAllianceMap blah blee >>= flip addUN blau

{-- 
>>> stuphfen (dear ++ moderns) (euDir ++ eu) (unDir ++ un)
...
>>> let stuf = it  
>>> Map.size stuf
44
--}

--- Now you:

parseOAS :: String -> Maybe Country
parseOAS line = pack . fst . break (=='}') <$> stripPrefix "|{{Country|" line

-- parseOAS, given a line, parses `|{{Country|Barbados}} [... and stuff]`

exOASbon, exOASmal :: String
exOASbon = concat ["|{{Country|Antigua and Barbuda}} || [[St. John's, Antigua ",
                   "and Barbuda|St. John's]] || 1981 || 1.78 || $20,977 || ",
                   "0.780 || [[Eastern Caribbean dollar]] || [[English ",
                   "language|English]]"]

{--
>>> parseOAS exOASbon 
Just "Antigua and Barbuda"
--}

exOASmal = concat ["! Nation !! Capital !! Year<br>joined !! [[List of ",
                   "countries by GDP (PPP)|GDP (PPP)<br>$Million]] !! [[List of ",
                   "countries by GDP (PPP) per capita|GDP (PPP)<br>per capita]] ",
                   "<ref name=\"A1\">{{cite news| first=| last=| title= World ",
                   "Bank | work= | date= 8 July 2014| url=http://data.worldbank",
                   ".org/indicator/NY.GDP.PCAP.PP.CD?order=wbapi_data_value_2013",
                   "+wbapi_data_value+wbapi_data_value-last&sort=desc}}</ref> !! ",
                   "[[List of countries by Human Development Index|HDI]]<br>",
                   "<small>2018 est.<br>for 2017</small><ref name=\"HDI\">{{cite ",
                   "web |url=http://hdr.undp.org/sites/default/files/2018_",
                   "human_development_statistical_update.pdf |title=2018 Human ",
                   "Development Report |year=2018 |accessdate=14 September ",
                   "2018 |publisher=United Nations Development Programme }}",
                   "</ref> !! [[Currency]] !! [[Official Language|Official<br>",
                   "language]]s"]

{--
>>> parseOAS exOASmal
Nothing
--}

-- which means you can now write:

oasParser :: FilePath -> IO Alliance
oasParser file = Alliance "Organization of American States" Set.empty
                      . Set.fromList . mapMaybe parseOAS . lines
                    <$> readFile file

{--
>>> oasParser (oasDir ++ oas)
Alliance {name = "Organization of American States", aliases = fromList [], 
          countries = fromList ["Antigua and Barbuda","Argentina","Bahamas",
               "Barbados","Belize","Bolivia","Brazil","Canada","Chile",
               "Colombia","Costa Rica","Cuba","Dominica","Dominican Republic",
               "Ecuador","El Salvador","Grenada","Guatemala","Guyana","Haiti",
               "Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay",
               "Peru","Saint Kitts and Nevis","Saint Lucia",
               "Saint Vincent and the Grenadines","Suriname",
               "Trinidad and Tobago","United States","Uruguay","Venezuela"]}

>>> let os = it
>>> Set.size (A.countries os)
35

$ grep -i "{{Country|" Y2020/M11/D10/org-american-states.wtxt| wc
      35     848    675

... so that checks out. :D 
--}

addOAS :: AllianceMap -> FilePath -> IO AllianceMap
addOAS = adder oasParser

{--
>>> addOAS stuf (oasDir ++ oas)
...
>>> let oses = it
>>> Map.size oses
45
--}

-- DOIT! TOIT!

-- So, our Big Kahuna for military alliances of the world is definitively:

go :: IO AllianceMap
go = stuphfen (dear ++ moderns) (euDir ++ eu) (unDir ++ un) >>=
     flip addOAS (oasDir ++ oas)

{--
>>> go
...
>>> let gone = it
>>> Map.size gone
45
--}
