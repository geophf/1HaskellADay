module Y2017.M01.D11.Exercise where

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
mbHead = undefined
mbLast = undefined

mbTail, mbInit :: [a] -> Maybe [a]
mbTail = undefined
mbInit = undefined

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

-- There! Total functions on lists! Feel better now?

-- Tomorrow, we'll look at partial functions on Foldable t: maximum, minimum
