module Control.Logic.Frege where

import Control.Arrow
import Control.Monad
import Data.Monoid

infixr 7 <<-
infixr 2 -|

-- provides a logic implication, yielding Maybe; all very Fregescque

(-|) :: Bool -> a -> Maybe a
True -| m = Just m
False -| m = Nothing

-- Now we lift implication

(=|) :: Maybe Bool -> a -> Maybe a
p =| q = p >>= (-| q)

{-- e.g.:

*Control.Logic.Frege> 3 > 4 -| Just "so" ~> Nothing
*Control.Logic.Frege> 3 > 1 -| Just "so" ~> Just "so"

A bit more elaborate example:

verify (Rel (B b) (SzQ g) Sibling:rest) ont@(ages, rels) =
   guard (getAll (legit b g rels)) >> verify rest ont
-- TODO: for Older

-- Is the engagement legit? That is, are there no relationships that are
-- incestual?
legit :: Boy -> Girl -> [Couple] -> All
legit brother sister [] = All True
legit brother sister (Rel b g Engaged:rels) =
   (b /= brother -| All (g /= sister)) `mappend` legit brother sister rels

The All-type is from Data.Monoid
--}

-- for the all-too-common f (g a b) pattern:

(<<-) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f <<- g = (f .) . g

-- distributing f over a same-kind tupled pair

adjoin :: Arrow a => a b c -> a (b, b) (c, c)
adjoin = join (***)

-- uncurry g . curry (adjoin f) occurs so often it needs it's own function

natx :: (b -> b -> c) -> (a -> b) -> a -> a -> c
natx g f = uncurry g <<- curry (adjoin f)

-- This common function just so happens to be called a natural transformation!

-- Now for the pattern monad m >>= \x -> guard (p x) >> return x

assert :: MonadPlus m => (a -> Bool) -> a -> m a
assert p x = guard (p x) >> return x

-- cut-operators for Boolean logic.

-- N.B.: cut impacts not the return values but may affect (monadic) effects!

mor, mand :: Monad m => m Bool -> m Bool -> m Bool
mor ma mb = ma >>= \bool -> if bool then ma else mb
mand ma mb = ma >>= \bool -> if bool then mb else ma
