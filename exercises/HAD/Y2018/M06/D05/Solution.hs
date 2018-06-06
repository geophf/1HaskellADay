{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Y2018.M06.D05.Solution where

{--
Another day, another data structure.

We have a smart-tagging algorithm that saves its results to JSON. We want to
take those results in store them in a database. But before we do that, we need
to parse the JSON into Haskell structures because Haskell structures are ...

... cute?
--}

import Control.Arrow ((&&&))
import Control.Monad ((>=>))

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Entity a = Entity { name :: String, wiki :: WikiInfo, related :: [a] }
   deriving (Eq, Show)

{--
Ugh, can't figure this out now

instance FromJSON a => FromJSON (Entity a) where
   parseJSON (Object o) =
      Entity <$> o .: "Name" <*> (o >>= parseJSON) <*> o .: "Related Articles"

so I'll do this instead:
--}

data SubEntity a = Sub { subname :: String, subrelated :: [a] }
   deriving (Eq, Show)

instance FromJSON a => FromJSON (SubEntity a) where
   parseJSON (Object o) = Sub <$> o .: "Name" <*> o .: "Related Articles"

data WikiInfo = Wiki { wikiname, wikisummary :: String,
                       wikiimages :: [FilePath], wikilink :: FilePath }
   deriving (Eq, Show)

instance FromJSON WikiInfo where
   parseJSON (Object o) =
      Wiki <$> o .: "WikiName"   <*> o .: "WikiSummary" 
           <*> o .: "WikiImages" <*> o .: "WikiLink"

data NamedWikiInfo = NWI { entityName, nwiname, nwisumary :: String,
                           nwiimages :: [FilePath], nwilink :: FilePath }
   deriving (Eq, Show)

instance FromJSON NamedWikiInfo where
   parseJSON (Object o) =
      NWI <$> o .: "Name" <*> o .: "WikiName"   <*> o .: "WikiSummary" 
          <*> o .: "WikiImages" <*> o .: "WikiLink"
     
-- the entities are stored here

exDir, entitiesFile :: FilePath
exDir = "Y2018/M06/D05/"
entitiesFile = "smart_tagging.json"

data SubEntities a = SE { elst :: [SubEntity a] }
   deriving Show

instance FromJSON a => FromJSON (SubEntities a) where
   parseJSON (Object o) = SE <$> o .: "Entities"

data SubWikis = SW { wlst :: [NamedWikiInfo] }
   deriving Show

instance FromJSON SubWikis where
   parseJSON (Object o) = SW <$> o .: "Entities"

readEntities :: FilePath -> IO [Entity Value]
readEntities = BL.readFile >=> \file ->

-- must do this as a two-pass compilation to get from sub-entities to entities

   let subents = fromJust (decode file)
       subwiks = fromJust (decode file) in
   return (subsToEntities (elst subents) (wlst subwiks))

subsToEntities :: [SubEntity a] -> [NamedWikiInfo] -> [Entity a]
subsToEntities (Map.fromList . map (subname &&& id) -> subs) =
   map (\nwi -> sub2Ent (subs Map.! (entityName nwi)) nwi)

sub2Ent :: SubEntity a -> NamedWikiInfo -> Entity a
sub2Ent (Sub n r) (nwi2wi -> wiki) = Entity n wiki r

-- which means we convert nwi to wi

nwi2wi :: NamedWikiInfo -> WikiInfo
nwi2wi (NWI _ n s i l) = Wiki n s i l

-- How many entities are there?
-- What is the name of the entity that has the most related articles?
-- How many related articles does it have?

{--
>>> json <- readEntities (exDir ++ entitiesFile)
>>> length json
12
>>> maximumBy (compare `on` snd) (map (name &&& length . related) json)
("USNS City of Bismarck",3)

... actually they all have three related articles, so that's a funny question
--}

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
   toJSON entity =
      object ["Name" .= name entity,
              "Related Articles" .= related entity,
              "Wiki" .= wiki entity]

instance ToJSON WikiInfo where
   toJSON wiki =
      object ["Name" .= wikiname wiki,
              "Summary" .= wikisummary wiki,
              "Images" .= wikiimages wiki,
              "Link" .= wikilink wiki]

-- Then rebuke the bad JSON by writing out that good JSON to file

data Entities a = Ents [Entity a]

instance ToJSON a => ToJSON (Entities a) where
   toJSON (Ents x) = object ["Entities" .= x]

writeEntities :: ToJSON a => FilePath -> [Entity a] -> IO ()
writeEntities output = BL.writeFile output . encodePretty . Ents

{--
>>> writeEntities (exDir ++ "smart_tagging_new.json") json

And you see the new JSON.
--}
