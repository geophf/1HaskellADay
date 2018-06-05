{-# LANGUAGE OverloadedStrings #-}

module Y2018.M06.D05.Exercise where

{--
Another day, another data structure.

We have a smart-tagging algorithm that saves its results to JSON. We want to
take those results in store them in a database. But before we do that, we need
to parse the JSON into Haskell structures because Haskell structures are ...

... cute?
--}

import Data.Aeson

data Entity a = Entity { name :: String, wiki :: WikiInfo, related :: [a] }
   deriving (Eq, Show)

instance FromJSON a => FromJSON (Entity a) where
   parseJSON (Object o) = undefined

data WikiInfo = Wiki { wikiname, wikisummary :: String,
                       wikiimages :: [FilePath], wikilink :: FilePath }
   deriving (Eq, Show)

instance FromJSON WikiInfo where
   parseJSON (Object o) = undefined

-- the entities are stored here

exDir, entitiesFile :: FilePath
exDir = "Y2018/M06/D05/"
entitiesFile = "smart_tagging.json"

readEntities :: FilePath -> IO [Entity Value]
readEntities file = undefined

-- How many entities are there?
-- What is the name of the entity that has the most related articles?
-- How many related articles does it have?

{-- PART DUEX! --------------------------------------------------------------

Okay. Look at the structure of the JSON.

Why do people do this? That is to say. They have entity information: great.
They have wiki information that they get from a separate call: great.

But why flatten into a pancake the wiki information with the entity information,
commingling the two?

Somebody ought to write a book: "Badly Structured Data, and How to Avoid it!"
or: "Good Data Structures."

Oh, somebody has written a book? Several somebodies?

Huh. It's like people look at JSON and are like: "I know data structures because
I've seen JSON once! Hold my beer!"

This JSON.

So. Output the JSON in well-structured (hierarchical) form.
--}

instance ToJSON a => ToJSON (Entity a) where
   toJSON entity = undefined

-- Then rebuke the bad JSON by writing out that good JSON to file

writeEntities :: ToJSON a => FilePath -> [Entity a] -> IO ()
writeEntities output entities = undefined
