> module Y2018.M02.D06.OneLiner where

You have

> f :: a -> [b] -> [c]
> f a bs = undefined

But instead of just one a you have [a]

Define:

> g :: [a] -> [b] -> [c]
> g as bs = undefined

in terms of f

{--
Solutions:

Daniel @leptonyu 
g as bs = foldl (\xs a -&gt; f a bs ++ xs) [] as

ptdr_bot @m0rth0n 
g as bs = flip f bs =&lt;&lt; as

Victoria C @ToriconPrime 
g as bs = concat $ fmap ($ bs) (fmap f as)

matt @themattchan 
g = flip $ concatMap . flip f

Nicoλas @BeRewt 
g = flip (flip (>>=) . flip f) 

Or: 

g as bs = as >>= flip f bs

Sangeet Kar @sangeet_kar 
g = foldMap f
--}
