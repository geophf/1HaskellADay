{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Y2020.M07.D24.Solution where

import Control.Arrow ((&&&), second)

import Data.Aeson

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromJust)
import Data.Ord -- for Down

import Prelude hiding (Left)

import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Generics

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L

{--
Yesterday we looked at converting images. Today, I processed some images
in Amazon Rekognition and got its analysis back. Let's process that.

So, image recognition and classification is hit-or-miss, depending on the
domains the classifier cares about. I sent a couple of flower pics to the
recognizer, and it said, en effet, "Eh, flowers. Who cares."

Well, I care.

Apparently, I still need to pay for my coffee, despite my caring about flowers.

So, I set an image of some birds (cardinals), and the recognizer got all
excited and classified things left, right, and center.

I guess the classifer likes birds, then?

So, three images are in this directory, along with their processed results
as both images and JSON. For your perusal.

Today, we'll focus on the interesting response and gather some information
from that.
--}

imageProcessingDir :: FilePath
imageProcessingDir = "Y2020/M07/D24/"

birdsResponse :: FilePath
birdsResponse = imageProcessingDir ++ "cardinal-response.json"

-- read in the above JSON, process it, and answer the below questions

type Name = String
type Confidence = Double
type Width = Double
type Height = Double
type Left = Double
type Top = Double

data BoundingBox = BB Width Height Left Top
   deriving (Eq, Ord, Show, Generic)

instance FromJSON BoundingBox where
   parseJSON = withObject "BoundingBox" (\v ->
      BB <$> v .: "Width"
         <*> v .: "Height"
         <*> v .: "Left"
         <*> v .: "Top")

-- originally was a generic declaration, but see note below in testing.

data Instance = Inst Confidence BoundingBox
   deriving (Eq, Ord, Show, Generic)

instance FromJSON Instance where -- heh.
   parseJSON = withObject "Instance" (\v ->
      Inst <$> v .: "Confidence"
           <*> v .: "BoundingBox")

data Parent = Parent Name
   deriving (Eq, Ord, Show, Generic)

instance FromJSON Parent where
   parseJSON = withObject "Parenthood" (\v -> Parent <$> v .: "Name")

data Label = Entity { name    :: Name,
                      conf    :: Confidence,
                      insts   :: [Instance],
                      parents :: [Parent] }
   deriving (Eq, Ord, Show)

instance FromJSON Label where
   parseJSON = withObject "Label" (\v ->
      Entity <$> v .: "Name"
             <*> v .: "Confidence"
             <*> v .: "Instances"
             <*> v .: "Parents")

{--
>>> processImageResponse birdsResponse 
Nothing

Oops! So: how do we debug this problem? ... one layer at a time:

1. Does BoundingBox work?
--}

bbSample :: String
bbSample = unlines ["{",
                    "\"Width\": 0.1887630820274353,",
                    "\"Height\": 0.11798793822526932,",
                    "\"Left\": 0.6554882526397705,",
                    "\"Top\": 0.2186919003725052",
                    "}"]

{--
>>> (decode (L.pack bbSample )) :: Maybe (BoundingBox )
Just (BB 0.1887630820274353 0.11798793822526932
         0.6554882526397705 0.2186919003725052)

Sigh. Because the Names are Capitalized (tm) and not lowercase, like JSON
everywhere else in the world, we cannot use the generics to declare these
FromJSON instances.

Thank you, manager-dweebs at AWS for making this breath-taking decision.

Manager: "Well, JSON doesn't require named attributes to be lowercase, I can
upcase the names if I wanna."

Yes. Yes, you can.

Dweeb.

so ... now that works, but:

>>> processImageResponse birdsResponse 
Nothing

Dis iz gurna be ah slog.
--}

instSamp :: String
instSamp = unlines (["{", "\"BoundingBox\":"] ++ lines bbSample 
                 ++ [", \"Confidence\": 99.50123596191406", "}"])

{--
>>> (decode (L.pack instSamp)) :: Maybe Instance
Just (Inst 99.50123596191406 
           (BB 0.1887630820274353 0.11798793822526932 
               0.6554882526397705 0.2186919003725052))

... but still:

>>> processImageResponse birdsResponse 
Nothing

Oh, plus tiens!
--}

parentSamp :: String
parentSamp = unlines ["{", "\"Name\": \"Animal\"", "}"]

{--
>>> (decode (L.pack parentSamp )) :: Maybe Parent
Just (Parent "Animal")

so, now:
--}

labelSamp :: String
labelSamp = unlines (["{", "\"Name\": \"Bird\",", 
                           "\"Confidence\": 99.50123596191406,",
                           "\"Instances\": ["]
                     ++ lines instSamp
                     ++ ["], \"Parents\": ["]
                     ++ lines parentSamp
                     ++ ["] }"])

{--
>>> (decode (L.pack labelSamp  )) :: Maybe Label
Just (Entity "Bird" 99.50123596191406 
             [Inst 99.50123596191406 
                   (BB 0.1887630820274353 0.11798793822526932 
                       0.6554882526397705 0.2186919003725052)] 
             [Parent "Animal"])

So: that all works, but this shows I need a container for all the Label/Entity
values, called the Labels type.
--}

data Labels = Labels { labels :: [Label] }
   deriving (Eq, Show)

instance FromJSON Labels where
   parseJSON = withObject "Labels" (\v ->
      Labels <$> v .: "Labels")

processImageResponse :: FilePath -> IO [Label]
processImageResponse json =
   L.readFile json >>= return . labels . fromJust . decode

{--
>>> let daBirdz =processImageResponse birdsResponse 
>>> daBirdz
[Entity "Bird" 99.50123596191406 
        [Inst 99.50123596191406 (BB 0.1887630820274353 0.11798793822526932 ...),
         Inst 96.68358612060547 (BB 0.0935601145029068 0.07330977171659472 ...),
         ..],
        [Parent "Animal"],
 Entity "Animal" 99.50123596191406 [] [],
 Entity "Cardinal" 92.3027114868164 [] [Parent "Animal",Parent "Bird"],
 Entity "Bee Eater" 79.80430603027344 [] [Parent "Animal",Parent "Bird"],
 Entity "Nature" 67.54881286621094 [] [],
 Entity "Beak" 58.042728424072266 [] [Parent "Animal",Parent "Bird"],
 Entity "Outdoors" 56.922935485839844 [] []]

... okay. But that was the easy part.

*geophf faints.
--}

-- Now that you have the processed results:

-- 1. How many birds are there? Or, more generally: How many of each type of
-- thing are there in this image?

objectCounts :: [Label] -> Map Name Int
objectCounts = Map.fromList . map (name &&& (length . insts))

{--
>>> objectCounts daBirdz 
fromList [("Animal",0),("Beak",0),("Bee Eater",0),
          ("Bird",4),("Cardinal",0),("Nature",0),("Outdoors",0)]
--}

-- 2. What kind of birds are in the image? Or, more generally: what are the
-- taxonomies in these results? How do you even represent a taxonomy?

data Taxonomy = Taxi Confidence (Set Parent)
   deriving (Eq, Ord, Show)

label2tax :: Label -> Taxonomy
label2tax = Taxi <$> conf <*> Set.fromList . parents

taxonomies :: [Label] -> Map Name Taxonomy
taxonomies = Map.fromList . map (name &&& label2tax)

{--
>>> taxonomies daBirdz 
{("Animal",Taxi 99.50123596191406 {}),
 ("Beak",Taxi 58.042728424072266 {Parent "Animal",Parent "Bird"]}),
 ("Bee Eater",Taxi 79.80430603027344 {Parent "Animal",Parent "Bird"}),
 ("Bird",Taxi 99.50123596191406 {Parent "Animal"}),
 ("Cardinal",Taxi 92.3027114868164 {Parent "Animal",Parent "Bird"}),
 ("Nature",Taxi 67.54881286621094 {}),
 ("Outdoors",Taxi 56.922935485839844 {})}
--}

-- 3. From the taxonomies, find the bird taxonomy and determine the kind of
-- bird, ... probably you want to base your guess on the highest probability
-- (ranked first)

kindOf :: Name -> Map Name Taxonomy -> [(Name, Confidence)]
kindOf obj = sortOn (Down . snd) 
           . map (second confTax)
           . filter (Set.member (Parent obj) . parTax . snd)
           . Map.toList

confTax :: Taxonomy -> Confidence
confTax (Taxi c _) = c

parTax :: Taxonomy -> Set Parent
parTax (Taxi _ p) = p

{--
>>> kindOf "Bird" (taxonomies daBirdz)
[("Cardinal",92.3027114868164),
 ("Bee Eater",79.80430603027344),
 ("Beak",58.042728424072266)]
--}
