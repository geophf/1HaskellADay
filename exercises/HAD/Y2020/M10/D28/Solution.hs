{-# LANGUAGE OverloadedStrings #-}

module Y2020.M10.D28.Solution where

{--
I collected the military alliances of the world, and their member-nations, with
the following SPARQL query to wikidata.org:

# Military alliances
SELECT ?alliance ?allianceLabel ?country ?countryLabel ?memberOf
WHERE 
{
  ?alliance wdt:P31 wd:Q1127126.
  ?country wdt:P31 wd:Q6256.
  ?country ?memberOf ?alliance.
  SERVICE wikibase:label { 
    bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en".
  }
}

We also have alliances in opposition:

# alliances and oppositions
SELECT ?alliance1 ?alliance1Label ?alliance2 ?alliance2Label
WHERE 
{
  ?alliance1 wdt:P31 wd:Q1127126.
  ?alliance1 wdt:P461 ?alliance2.
  SERVICE wikibase:label { 
    bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en".
  }
}

but that result-set is rather underwhelming, should you care to look.

The military alliances and their member countries, and the oppositions
are stored in JSON files in this directory. Read in these JSONs in the data
structures declared below, then answer the questions following.
--}

import Y2020.M10.D12.Solution hiding (country)   -- for Country

import Control.Arrow ((&&&))

import Data.Aeson
import Data.Aeson.Types

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.List (sort)
import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as T

deer :: FilePath   -- ... geddit?
deer = "Y2020/M10/D28/"

alliancesFile :: FilePath
alliancesFile = "military-alliances.json"

-- okay, we need some structure for the above, yes?

type Qname = Text
type Name = Text

data WikiDatum = WD { qid :: Qname, name :: Name }
   deriving (Eq, Ord, Show)

{--

No. Not the time for this.

class Tagged t where
   tag :: t -> Text
--}

-- this is a ... 'little' (?) tricky, because we need to parse a datum by
-- its labels.

-- we introduce the *: operator

(*:) :: Object -> Text -> Parser WikiDatum
obj *: label = WD <$> obj .: label <*> obj .: (T.concat [label, "Label"])

-- and with this new operator, we can do this naturally:

type Alliance = Text

data AllianceMember = AllianceMember { alliance :: WikiDatum,
                                       country :: WikiDatum }
   deriving (Eq, Ord, Show)

instance FromJSON AllianceMember where
   parseJSON = withObject "AllianceMember" $ \v -> AllianceMember
     <$> v *: "alliance" <*> v *: "country"

-- also, please filter out the (one) alliance that does not have membership
-- value the same as all the other members (that's an indirect hint that
-- `memberOf` isn't always a `memberOf`-property).

-- let's try this out on a sample first:

samp :: ByteString
samp = BL.pack (concat ["{\"alliance\":\"http://www.wikidata.org/entity/Q7184\",",
       "\"allianceLabel\":\"NATO\",",
       "\"country\":\"http://www.wikidata.org/entity/Q142\",",
       "\"countryLabel\":\"France\"}"])

{--
>>> (decode samp) :: Maybe AllianceMember 
Just (AllianceMember {
        alliance = WD {qid = "http://www.wikidata.org/entity/Q7184", 
                       name = "NATO"}, 
        country  = WD {qid = "http://www.wikidata.org/entity/Q142", 
                       name = "France"}})

WOOT!
--}

allianceMembers :: ByteString -> [AllianceMember]
allianceMembers = fromMaybe [] . decode

{--
>>> BL.readFile (deer ++ alliancesFile)
...
>>> let af = it
>>> let allis = allianceMembers af
>>> length allis
42
>>> head allis
AllianceMember {alliance = WD {qid = "http://www.wikidata.org/entity/Q7184", 
                               name = "NATO"},
                country = WD {qid = "http://www.wikidata.org/entity/Q30", 
                              name = "United States of America"}}
--}

-- We're going to have to explore alliances further, as wikidata is woefully
-- underrepresentative, i.e.:

-- https://en.wikipedia.org/wiki/List_of_military_alliances

-- But these alliances do have (enhanced) Country-data. How many countries
-- are mentioned in all the alliances?

countries :: [AllianceMember] -> Set Country
countries = Set.fromList . map (name . country)

{--
>>> countries allis
fromList ["Australia","Bulgaria","Canada","Croatia","Denmark","France",
          "Germany","Italy","Kazakhstan","Kingdom of Denmark","Kyrgyzstan",
          "Latvia","Montenegro","Netherlands","New Zealand","Norway","Pakistan",
          "Philippines","Poland","Portugal","Romania","Slovakia","Tajikistan",
          "Thailand","Turkey","United Kingdom","United States of America",
          "Uzbekistan"]
--}

-- How many alliances are there?

alliances :: [AllianceMember] -> Set Alliance
alliances = Set.fromList . map (name . alliance)

{--
>>> alliances allis
fromList ["Anzus","Collective Security Treaty Organisation","Five Eyes","NATO",
          "Southeast Asia Treaty Organization","Warsaw Pact",
          "Western European Union","polla"]
--}

{-- BONUS -------------------------------------------------------

Read in the oppositions. How many oppositions are there?

... n.b.: Opposition a b === Opposition b a

Also: Please explain to me why Poland is in opposition to Poland? For I seek
knowledge to learn and to know.

Are there other such oppositions?
--}

oppositionsFile :: FilePath
oppositionsFile = "oppositions.json"

data Opposition = Opposition { alliance1, alliance2 :: WikiDatum }
   deriving Show

instance Eq Opposition where
   Opposition a b == Opposition c d =
      a == c && b == d || a == d && b == c

instance Ord Opposition where
   compare (Opposition a b) (Opposition c d) = -- compare (a,b) (c,d)
      -- the above doesn't work: we have to order ab and cd first then compare
      let ab = sort [a,b]
          cd = sort [c,d]
      in compare ab cd

instance FromJSON Opposition where
   parseJSON = withObject "Opposition" $ \v -> Opposition
      <$> v *: "alliance1" <*> v *: "alliance2"

oppositions :: ByteString -> Set Opposition
oppositions = Set.fromList . fromMaybe [] . decode

{--
>>> BL.readFile (deer ++ oppositionsFile)
>>> let ofil = it
>>> let opz = oppositions ofil
>>> Set.size opz
2
--}

weirdOppositions :: Map Alliance (Set Country) -> Set Opposition -> Set Country
weirdOppositions m = Set.unions . map (wo' m) . Set.toList

wo' :: Map Alliance (Set Country) -> Opposition -> Set Country
wo' m (Opposition a b) = Set.intersection (membersOf a) (membersOf b)
   where membersOf x = fromMaybe Set.empty (Map.lookup (name x) m)

-- returns a set of countries that oppose themselves.

-- but to do weirdness we have to map alliances to countries:

memberCountries :: [AllianceMember] -> Map Alliance (Set Country)
memberCountries = foldr (uncurry (alter1 f)) Map.empty
                . map (name . alliance &&& name . country)
   where f Nothing = Just . Set.singleton
         f (Just s) = Just . flip Set.insert s
         alter0 :: Ord k => (Maybe (Set v) -> v -> Maybe (Set v))
                         -> k -> Map k (Set v) -> v -> Map k (Set v)
         alter0 f k m0 = maybe m0 (flip (Map.insert k) m0) . f (Map.lookup k m0)
         alter1 f = flip . alter0 f

{--
>>> let mc = memberCountries allis
>>> take 3 $ Map.toList mc
[("Anzus",fromList ["Australia","New Zealand"]),
 ("Collective Security Treaty Organisation",fromList ["Kazakhstan","Kyrgyzstan",
                                                      "Tajikistan","Uzbekistan"]),
 ("Five Eyes",fromList ["Australia","Canada","New Zealand","United Kingdom"])]

>>> weirdOppositions mc opz
fromList ["Bulgaria","Poland","Romania"]
--}
