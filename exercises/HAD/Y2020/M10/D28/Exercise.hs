{-# LANGUAGE OverloadedStrings #-}

module Y2020.M10.D28.Exercise where

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

import Y2020.M10.D12.Exercise   -- for Country

import Data.Aeson
import Data.Aeson.Types

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Set (Set)
import Data.Text (Text)

deer :: FilePath   -- ... geddit?
deer = "Y2020/M10/D28/"

alliancesFile :: FilePath
alliancesFile = "military-alliances.json"

-- okay, we need some structure for the above, yes?

type Qname = Text
type Name = Text

data WikiDatum = WD { qid :: Qname, name :: Name }
   deriving (Eq, Ord, Show)

-- this is a ... 'little' (?) tricky, because we need to parse a datum by
-- its labels.

{--
instance FromJSON WikiDatum where
   parseJSON = undefined

This --^ doesn't work because we need to know the label at the time of
parse. Let's, instead, create a 'smart'-parser that uses the entire arrow:
--}

(*:) :: Object -> Text -> Parser WikiDatum
obj *: label = WD <$> undefined <*> undefined

-- and with this new operator, we can do this naturally:

data AllianceMember = AllianceMember { alliance :: WikiDatum,
                                       country :: WikiDatum }
   deriving (Eq, Ord, Show)

instance FromJSON AllianceMember where
   parseJSON = undefined

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
allianceMembers = undefined

-- We're going to have to explore alliances further, as wikidata is woefully
-- underrepresentative, i.e.:

-- https://en.wikipedia.org/wiki/List_of_military_alliances

-- But these alliances do have (enhanced) Country-data. How many countries
-- are mentioned in all the alliances?

countries :: [AllianceMember] -> Set Country
countries = undefined

-- How many alliances are there?

alliances :: [AllianceMember] -> Set Country
alliances = undefined

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
   (==) = undefined

instance Ord Opposition where
   compare = undefined       -- for Set insertion

instance FromJSON Opposition where
   parseJSON = undefined

oppositions :: ByteString -> Set Opposition
oppositions = undefined

weirdOppositions :: Set Opposition -> Set Country
weirdOppositions = undefined

-- returns a set of countries that oppose themselves.
