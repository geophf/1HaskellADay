{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2020.M10.D30.Solution where

{--
So, the wikipedia entry for all alliances of the world:

https://en.wikipedia.org/wiki/List_of_military_alliances

has a section on modern, or current, military alliances. These entries
are saved in the wikitext-format. The wikitext format is described here:

https://meta.wikimedia.org/wiki/Help:Wikitext_examples

... but is self-evident from the text, itself.

The listing of modern military alliances is archived here:
--}

import Control.Arrow ((&&&), (***), first, second)

import Data.List (stripPrefix, sortOn)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromMaybe)
import Data.Ord                  -- for Down

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as T

import Y2020.M10.D12.Solution hiding (workingDir)     -- for Country
import Y2020.M10.D14.Solution                         -- for ContinentMap
import Y2020.M10.D15.Solution (countryMap)
import Y2020.M10.D28.Solution hiding (Alliance, name, countries) -- for Name

dear :: FilePath
dear = "Y2020/M10/D30/"    -- ... geddit?

moderns :: FilePath
moderns = "modern-alliances.wtxt"

-- figure out how to scan and parse the above document into the below
-- data structure

type Alias = Text

data Alliance = Alliance { name :: Name, 
                           aliases :: Set Alias,
                           countries :: Set Country }
   deriving (Eq, Ord, Show)

type AllianceMap = Map Name Alliance

parseAlliances :: FilePath -> IO AllianceMap

{--
The mapMaybe approach doesn't work, because we aren't accepting or rejecting
alliances BY LINE, because aliases follow the alliance, line-by-line. So,
we need to parse an alliance on this line, then keep that context open to
parse, possibly, any aliases the alliance may have.

parseAlliances wikitext = 
   Map.fromList . map (name &&& id) . mapMaybe pa' . lines <$> readFile wikitext

pa' :: String -> Maybe Alliance
pa' = undefined
--}

-- (nice try, though. Come again, soon.)

parseAlliances wikitext = 
   Map.fromList . map (name &&& id) . pa' [] . lines <$> readFile wikitext

pa' :: [Alliance] -> [String] -> [Alliance]
pa' ans [] = ans
pa' acc l@(_:ines) = 
   maybe (pa' acc ines) (uncurry pa' . first (:acc)) (pa'' l)

pa'' :: [String] -> Maybe (Alliance, [String])
pa'' (l:ines) = pa''' l >>= \(n, cs) ->
   optionalAliases ines >>=
   return . first (flip (Alliance n) cs)

pa''' :: String -> Maybe (Name, Set Country)
pa''' line = 
   parseStar line       >>=
   consumeSpaces        >>=
   optionalFlagIcon     >>=
   allianceName         >>= \(n,r0) ->
   countryFlags r0      >>=
   return . (n,)

{-- 
Parsing alliances should be (once you get to an alliance block):

parseStar
parseOptionalSpace
optionallyParseAllianceFlag
parseAllianceName
parseEachAllianceCountry
optionallyParseAliases (on the new lines, designated by **)

With that, you should be able to retrieve each alliance.

--}

parseStar :: String -> Maybe String
parseStar ('*':rest) = Just rest
parseStar _          = Nothing

consumeSpaces :: String -> Maybe String
consumeSpaces [] = Nothing
consumeSpaces (' ':rest) = consumeSpaces rest
consumeSpaces elsewise = Just elsewise

optionalFlagIcon :: String -> Maybe String
optionalFlagIcon s = Just (fromMaybe s (snd <$> flagIcon s))

flagIcon :: String -> Maybe (Country, String)
flagIcon = unbox "{{flagicon|" '}'

allianceName :: String -> Maybe (Name, String)
allianceName = unbox "[[" ']'

unbox :: String -> Char -> String -> Maybe (Text, String)
unbox pre post s = (T.pack *** drop 2) . break (== post) <$> stripPrefix pre s

countryFlags :: String -> Maybe (Set Country)
countryFlags = Just . cf' Set.empty

cf' :: Set Country -> String -> Set Country
cf' acc s = maybe acc (uncurry (addCountry acc)) (consumeSpaces s >>= flagIcon)

addCountry :: Set Country -> Country -> String -> Set Country
addCountry acc c = cf' (Set.insert c acc)

-- countryFlags is a fun one. Let's take it for a spin:

flaggies :: String
flaggies = " {{flagicon|Estonia}} {{flagicon|Latvia}} {{flagicon|Lithuania}}"

{--
>>> countryFlags flaggies 
Just (fromList ["Estonia","Latvia","Lithuania"])
--}

optionalAliases :: [String] -> Maybe (Set Alias, [String])
optionalAliases = Just . (Set.empty,) -- TODO

-- let's try a whole line:

samp0 :: String
samp0 = "*[[Economic Community of Central African States]] {{flagicon|Angola}}"
     ++ " {{flagicon|Burundi}} {{flagicon|Cameroon}} {{flagicon|Chad}}"
     ++ " {{flagicon|Central African Republic}} {{flagicon|Democratic Republic"
     ++ " of the Congo}} {{flagicon|Gabon}} {{flagicon|Republic of the Congo}}"
     ++ " {{flagicon|Equatorial Guinea}} {{flagicon|Sao Tome and Principe}}"


{--
>>> pa''' samp0
Just ("Economic Community of Central African States",
      fromList ["Angola","Burundi","Cameroon","Central African Republic","Chad",
                "Democratic Republic of the Congo","Equatorial Guinea","Gabon",
                "Republic of the Congo","Sao Tome and Principe"])
--}

{--
So: all of that put together:

>>> parseAlliances (dear ++ moderns)
...
>>> let allis = it
--}

-- Except: --v

-- data clean-up --------------------------------------------------

{--
Okay, very first alliance, we have a problem. "African Union: all 58 African
nations are members."

What are the 58 African nations? Replace the free text with those nations.

Use the ContinentMap to guide you.
--}

updateAfricanUnion :: ContinentMap -> Continent -> Alliance -> Alliance
updateAfricanUnion cm conti alli@(Alliance n a _) =
   maybe alli (Alliance n a . Set.fromList) (Map.lookup conti cm)

{--
>>> countriesByContinent (workingDir ++ cbc)
>>> let m = it

>>> Map.lookup (T.pack "African Union") allis
Nothing

... it didn't parse, so:

>>> let uau = updateAfricanUnion m (T.pack "Africa") 
                         (Alliance (T.pack "African Union") Set.empty Set.empty)
Alliance {name = "African Union", 
          aliases = fromList [], 
          countries = fromList ["Algeria","Angola","Benin","Botswana",
                         "Burkina Faso","Burundi",
                         "Cameroon (also spelled Cameroun)","Cape Verde",
                         "Central African Republic","Chad (Tchad)","Comoros",
                         "C\244te d'Ivoire (Ivory Coast)",
                         "Democratic Republic of the Congo (Zaire)","Djibouti",
                         "Egypt (Misr)","Equatorial Guinea","Eritrea",
                         "Ethiopia (Abyssinia)","Gabon","Ghana","Guine",
                         "Guinea","Kenya","Lesotho","Liberia","Libya",
                         "Madagascar","Malawi","Mali","Mauritania","Mauritius",
                         "Morocco (Al Maghrib)","Mozambique","Namibia","Niger",
                         "Nigeria","Republic of the Congo","Rwanda","Senegal",
                         "Seychelles","Sierra Leone","Somalia","South Africa",
                         "South Sudan","Sudan","Swaziland (Eswatini)",
                         "S\227o Tom\233 and Pr\237ncipe","Tanzania",
                         "The Gambia","Togo","Tunisia","Uganda","Western Sahara",
                         "Zambia","Zimbabwe"]}

>>> let allis1 = Map.insert (T.pack "African Union") uau allis

Yay!-...ish.
--}

-- Okay. Now that we have the modern alliances

{-- 
1. How many alliances are there?

>>> Map.size allis1
23

-- 2.a. How many countries are in multiple alliances?
-- 2.b. list those countries with their multiple alliances

First we have to get all the countries:

>>> let ctries = Set.unions . map countries $ Map.elems allis1
>>> Set.size ctries
113
>>> take 3 $ Set.toList ctries 
["Angola","Argentina","Australia"]

Then, we reverse the map, sorting by the length of the associated alliances
--}

alliancesOf :: Country -> Map Name Alliance -> Set Alliance
alliancesOf country = Set.fromList . filter (countryIn country) . Map.elems

countryIn :: Country -> Alliance -> Bool
countryIn c = Set.member c . countries

countryAlliances :: Map Name Alliance -> Set Country -> Map Country (Set Alliance)
countryAlliances allis =
   Map.fromList . map (id &&& flip alliancesOf allis) . Set.toList

{--
>>> let ca = countryAlliances allis1 ctries 
>>> Map.size ca
113

>>> let multis = sortOn (Down . Set.size . snd) $ Map.toList ca
>>> take 5 $ map (second Set.size) multis 
[("Angola",2),("Argentina",2),("Benin",2),("Brazil",2),("Burkina Faso",2)]

3. What Countries here are not in the CountryMap?

>>> Set.difference (Map.keysSet ca) (Map.keysSet cm)
fromList ["Brazil","Cabo Verde","Cambodia","Cameroon","Chad",
          "Democratic Republic of the Congo","Dominican Republic",
          "Guinea-Bissau","Hezbollah","Italy","Ivory Coast","Jordan","Morocco",
          "Myanmar","Netherlands","Palestine","Panama","Sao Tome and Principe",
          "Spain","Thailand","Timor_Leste","UK","USA","Ukraine"]

>>> Set.size $ Set.difference (Map.keysSet ca) (Map.keysSet cm)
24
--}
