module Y2018.M10.D19.Exercise where

import Data.Aeson
import Data.Map (Map)
import Data.Set (Set)

{--
The categories.txt file contains a set of categories. A category is a
type/descriptor. If we look at the category types, they can be succinctly 
described in the following table:


id      cat_typ parent
----------------------
16	primary	
17	channel	16
18	section	17
19	link	17
20	display	
21	channel	20
22	section	20
23	channel	
24	section	23
25	link	23
26	stock	
27	symbol	26

Today's Haskell exercise is to verify this.

So:
--}

exDir, catFile :: FilePath
exDir = "Y2018/M10/D18/"
catFile = "categories.txt"

type Category = String

readCats :: FilePath -> [Category]
readCats file = undefined

-- how many unique categories are in the file? (hint: Set)

-- Now define and read in the category types

data CategoryType = CatT Int String (Maybe Int)
   deriving (Eq, Ord, Show)

instance FromJSON CategoryType where
   parseJSON = undefined

-- note that the indices are String in the file, so convert them to Int

type CatDict = Map Int CategoryType

readCatTypes :: FilePath -> IO CatDict
readCatTypes file = undefined

-- Now, given a CategoryType, be it anywhere in that graph, reconstruct
-- the full category

catType :: Int -> CatDict -> Maybe Category
catType idx dict = undefined

{--
So:

>>> dict <- readCatTypes (exDir ++ catTypes)

>>> catType 26 dict
Just "stock"

>>> catType 19 dict
Just "primary-channel-link"

>>> catType 1 dict
Nothing
--}

-- Now, verify the reverse. From a Category, find its leaf index

catTypeIndex :: Category -> CatDict -> Maybe Int
catTypeIndex str dict = undefined

{--
>>> catTypeIndex "primary-channel-section" dict
Just 18

>>> catTypeIndex "bleh" dict
Nothing
--}
