module Y2020.M10.D27.Exercise where

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

{-- BONUS -------------------------------------------------------

Upload the alliances, their member countries, and their oppositions to
the graph database.
--}

{-- BONUS-BONUS -------------------------------------------------

Update the countries with their respective item ids from wikidata.
--}
