{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.WikiDatum where

import Data.Aeson
import Data.Aeson.Types

import Data.Text (Text)
import qualified Data.Text as T

type Qname = Text
type Name = Text

data WikiDatum = WD { qid :: Qname, name :: Name }
   deriving (Eq, Ord, Show)

-- this is a ... 'little' (?) tricky, because we need to parse a datum by
-- its labels.

-- we introduce the *: operator

(*:) :: Object -> Text -> Parser WikiDatum
obj *: label = WD <$> obj .: label <*> obj .: (T.concat [label, "Label"])

{-- ... and with this new operator, we can do this naturally:

type Alliance = Text

data AllianceMember = AllianceMember { alliance :: WikiDatum,
                                       country :: WikiDatum }
   deriving (Eq, Ord, Show)

instance FromJSON AllianceMember where
   parseJSON = withObject "AllianceMember" $ \v -> AllianceMember
     <$> v *: "alliance" <*> v *: "country"

-- let's try this out on a sample first:

samp :: ByteString
samp = BL.pack (concat ["{\"alliance\":\"http://www.wikidata.org/entity/Q7184\",",
       "\"allianceLabel\":\"NATO\",",
       "\"country\":\"http://www.wikidata.org/entity/Q142\",",
       "\"countryLabel\":\"France\"}"])

>>> (decode samp) :: Maybe AllianceMember 
Just (AllianceMember {
        alliance = WD {qid = "http://www.wikidata.org/entity/Q7184", 
                       name = "NATO"}, 
        country  = WD {qid = "http://www.wikidata.org/entity/Q142", 
                       name = "France"}})
WOOT!

implementation from Y2020.M10.D28.Solution.hs
--}


