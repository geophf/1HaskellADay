module Graph.JSON.Cypher.Read where

-- We provide helper functions to read JSON values

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

-- Some helper functions to extract Value-values and unbox while extracting

(<<-$) :: Ord a => Map a Value -> a -> Maybe String
x <<-$ y = fmap unboxs (Map.lookup y x)

unboxs :: Value -> String
unboxs (String s) = T.unpack s

(<<-#) :: (Ord a, Integral b) => Map a Value -> a -> Maybe b
x <<-# y = fmap unboxn (Map.lookup y x)

unboxn :: Integral a => Value -> a
unboxn (Number n) = floor (read $ show n)

{--
Example of use:

tweetFrom :: PropertiesJ -> Tweet
tweetFrom (PJ props) = 
   fromJust (Tweet <$> props <<-$ "id_str" <*> props <<-$ "text"
                   <*> props <<-$ "created_at" <*> props <<-# "favorites")
--}
