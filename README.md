# 1 Haskell A Day exercises

This project provides a git support for 1HaskellADay's exercises
(see [@1HaskellADay](http://twitter.com/1HaskellADay) and the hashtag [#1HaskellADay](https://twitter.com/search?q=%231HaskellAday&src=hash))

## Getting started

- Find an exercise in the `exercises` folder (there is one in the `Exercise.hs`
  file of each "leaf/date folder").  For example, the first one is in
  `HAD\Y2014\M02\D24\Exercise.hs`
- Do it.
- At the root of the project, run `ghci`.
- check your answer with `check`. For example here: `check 2014 2 24`
- compare your answer with the proposed one (in the `Solution.hs` file)

### The 1had executable

If you install 1HaskellADay with cabal (`cabal configure` and then
`cabal install`), you can use the `1had` executable for an easier access
to the tests and exercices.
