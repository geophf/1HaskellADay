> module Y2018.M02.D06.OneLiner where

You have

> f :: a -> [b] -> [c]
> f a bs = undefined

But instead of just one a you have [a]

Define:

> g :: [a] -> [b] -> [c]
> g as bs = undefined

in terms of f
