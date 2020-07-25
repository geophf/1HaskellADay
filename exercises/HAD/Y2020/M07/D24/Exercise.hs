module Y2020.M07.D24.Exercise where

import Data.Aeson

import Data.Map (Map)
import qualified Data.Map as Map

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

data ImageResponse a = IR a

instance FromJSON a => FromJSON (ImageResponse a) where
   parseJSON = undefined

-- you have to clarify the declaration of the placeholder-type a

processImageResponse :: FilePath -> IO (Maybe (ImageResponse a))
processImageResponse json = undefined

-- Now that you have the processed results:

-- 1. How many birds are there? Or, more generally: How many of each type of
-- thing are there in this image?

objectCounts :: ImageResponse a -> Map String Int
objectCounts results = undefined

-- 2. What kind of birds are in the image? Or, more generally: what are the
-- taxonomies in these results? How do you even represent a taxonomy?

data Taxonomy = SomeKindOfTree String

taxonomies :: ImageResponse a -> Map String Taxonomy
taxonomies results = undefined

-- 3. From the taxonomies, find the bird taxonomy and determine the kind of
-- bird, ... probably you want to base your guess on the highest probability
-- (ranked first)

type Probability = Double

kindOf :: String -> Map String Taxonomy -> [(String, Probability)]
kindOf obj taxonomyMap = undefined
