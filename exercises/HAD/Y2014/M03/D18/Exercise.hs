module HAD.Y2014.M03.D18.Exercise where

-- $setup
-- >>> import Data.Maybe
-- >>> let backPartner = (>>= partner) . (>>= partner)

data Person a = Single a | Married a (Person a)

partner :: Person a -> Maybe (Person a)
partner (Married _ p) = Just p
partner _ = Nothing

get :: Person a -> a
get (Single x)    = x
get (Married x _) = x

-- | wedding
-- Marry single people, linking them together
-- Nothing if one is married
--
-- If you're used to Haskell, this one should be VERY easy.
-- But remember how strange it was the first time...
-- And see you tomorrow!
--
-- Examples:
--
-- >>> isNothing $ wedding (Married "foo" (Single "foobar")) (Single "bar")
-- True
--
-- prop> \(x,y) -> (fmap get . backPartner . fmap fst $ wedding (Single x) (Single y)) == Just (x :: String)
-- prop> \(x,y) -> (fmap get . backPartner . fmap snd $ wedding (Single x) (Single y)) == Just (y :: String)

wedding :: Person a -> Person a -> Maybe (Person a, Person a)
wedding = undefined
