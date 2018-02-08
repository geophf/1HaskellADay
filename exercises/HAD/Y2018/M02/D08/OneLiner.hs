module Y2018.M02.D08.OneLiner where

{--
We have maybe :: b -> (a -> b) -> Maybe a -> b

But we don't have list? Or do we? Define list:
--}

list :: b -> ([a] -> b) -> [a] -> b
list nullanswer flist lst = undefined

{-- BONUS -----------------------------------------------------------------

Both Maybe a and [a] are binary types, ... so is MonadPlus:

Maybe a = Nothing | Just a
List a = Cons a (List a) | Nil
MonadPlus m => mzero | m a `mplus` m a

Is there some generalization that maybe and list are functions of?

What is that generalization?
--}
