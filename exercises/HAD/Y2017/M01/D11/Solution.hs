module Y2017.M01.D11.Solution where

{--
Continuing along with Haskell Pitfalls from @bitemyapp, let's look at partial
functions in the Prelude, particularly for the hated, evil LIST.

WE HATE LIST! YES, WE DO! WE HATE LIST! HOW ABOUT YOU!

Actually I'm a LISP-guy, so, by definition, I'm a List-guy. SOMEtimes I use
list when PERHAPS I could use something else ...? Perhaps because it works
so well and so easily for so many problems? Perhaps?

But I digress.

So, let's look at head, tail, init and last.

These are partial functions because they fail for []:

*hask> tail []
*** Exception: Prelude.tail: empty list

So, that's the problem. Another problem is with the Maybe-type when you call
fromJust when there's no JUSTice:

*hask> fromJust Nothing
*** Exception: Maybe.fromJust: Nothing

But the solution for that is not to use fromJust, but use other maybe-functions
(e.g.: fromMaybe or maybe)

So, I'll just skip the fromJust-issue, and focus today only on List-functions.

Okay.

So, head, tail, init, and last are partial functions on List (that can be []).

How do we solve this partialiality issue for these List functions?

Well, have these functions work only an a new type: NonEmptyList

OR have these functions unpartialized, or totalled, by returning Maybe-values?

What to do?

Well, introducting a new type NonEmptyList is going to have a LOT of buy-in,
isn't it, like the wholesale adoption of Text and ByteString (ByteArray, more 
like) over the 'inefficient' String, right?

*cough*

Well, it is one way to go.

The other way is to en-maybe-ify these functions. Let's do that:
--}

mbHead, mbLast :: [a] -> Maybe a
mbHead [] = Nothing
mbHead (h:t) = Just h
mbLast [] = Nothing
mbLast list@(_:_) = Just (last list)  -- a total fn that uses a partial fn

mbTail, mbInit :: [a] -> Maybe [a]
mbTail [] = Nothing
mbTail (h:t) = Just t
mbInit [] = Nothing
mbInit list@(_:_) = Just (init list)  -- sounds legit

{--
Okay, with your definitions of the above TOTAL functions, what are the values
of:

mbHead, mbLast, mbTail, mbInit

for the following values:
--}

nada, uno, buncha :: [Int]
nada   = []
uno    = [1]
buncha = [1..10]

{--
*Y2017.M01.D11.Solution> [mbHead, mbLast] <*> [nada, uno, buncha] ~>
[Nothing,Just 1,Just 1,
 Nothing,Just 1,Just 10]

*Y2017.M01.D11.Solution> [mbTail, mbInit] <*> [nada, uno, buncha] ~>
[Nothing,Just [],Just [2,3,4,5,6,7,8,9,10],
 Nothing,Just [],Just [1,2,3,4,5,6,7,8,9]]
--}

-- There! Total functions on List! Feel better now?
