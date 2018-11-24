module Y2018.M11.D23.Solution where

{--
A puzzle game from

https://www.logic-puzzles.org/game.php?u2=63d3ded6b70a71eac63ba911fadacc6e

The Springfield County Bird Club met today, and several of its members brought
their newest acquisitions. Match each girl to her newest bird - determine its 
species and the month in which it was purchased.


1. Ida's pet was bought 1 month before the parrot.
2. Tamara's pet, the lorikeet and the bird bought in February are all different birds.
3. The finch was bought 1 month before Ida's pet.
4. The bird bought in February is either the finch or Ellen's pet.
5. Ellen's pet is either the canary or the bird bought in February.

Here are the data values under consideration:
--}

import Control.Monad (guard)

import Data.List (delete)
import Data.Set (Set)
import qualified Data.Set as Set

data Month = January | February | March | April
   deriving (Eq, Ord, Show, Enum)

data Bird = Canary | Finch | Lorikeet | Parrot
   deriving (Eq, Ord, Show, Enum)

data Girl = Alberta | Ellen | Ida | Tamara
   deriving (Eq, Ord, Show, Enum)

data Answer = Ans { girl :: Girl, bird :: Bird, month :: Month }
   deriving (Eq, Ord, Show)

-- how do you represent a Clue as a Haskell term?

type Clue = Set Answer

clues :: [Clue]
clues =

-- Tamara's pet
-- 2. Tamara's pet, the lorikeet and the bird bought in February are all 
-- different birds

-- 2a. Tamara's pet is not a lorikeet
-- 2b. Tamara's pet was not bought in February

    [ Ans Tamara pet month | pet <- [Canary, Finch, Parrot],
                             month <- [January,March,April]] >>=
    \t@(Ans Tamara tpet tmos) -> 

-- 2c. The lorikeet is not owned by Tamara and is not a February bird

-- Which means we know that when Tamara's bird was bought, we can narrow
-- down when the lorikeet was not bought.

    [ Ans owner Lorikeet month | owner <- [Alberta, Ellen, Ida],
                                 month <- delete tmos [January, March, April]] >>=
    \l@(Ans lown Lorikeet lmos) ->

-- 1. and 3. finch -> Ida's pet -> parrot
-- this means Ida's pet is neither a finch nor a parrot

-- Also, this means that once we've fixed the Finch's owner, we can
-- narrow down the parrot's owner.

    [ Ans owner Finch month | owner <- delete lown [Alberta, Ellen, Tamara],
                              month <- delete lmos [January, February]] >>=
    \f@(Ans fown Finch fmos) ->
    [ Ans Ida bird (succ fmos) | bird <- delete tpet [Canary, Lorikeet]] >>=
    \i@(Ans Ida ipet imos) ->
    [ Ans owner Parrot (succ imos) | owner <- delete lown (delete fown [Alberta, Ellen, Tamara])] >>=
    \p@(Ans pown Parrot pmos) ->

-- 4. and 5. February bird: finch or Ellen's pet
--           Ellen's pet: canary or February

-- That means February's bird is either the canary or the finch?
-- No, that means Ellen's pet is not a finch

   [Ans Ellen pet month | pet <- delete tpet (delete ipet [Canary,Lorikeet,Parrot]),
                          month <- delete tmos (delete fmos (delete imos [January .. April]))] >>=
   \e ->

-- now we have t l f i p e ... does that form a word?

-- wrong question to ask.

-- The correct question is that some of these answers are the same. Let's
-- elminate the duplicates and return them as a set.

   let ans = Set.fromList [t,l,f,i,p,e] in
   guard (length ans == 4) >> return ans

-- and given the above clues, solve the puzzle

hasAll4 :: Eq a => (Answer -> a) -> [a] -> Clue -> Bool
hasAll4 f as = ha4 f as . Set.toList

ha4 :: Eq a => (Answer -> a) -> [a] -> [Answer] -> Bool
ha4 _ [] [] = True
ha4 _ (_:_) [] = False
ha4 _ [] (_:_) = False
ha4 f (x:xs) (ans:anss) =
   ha4 f xs (if x == f ans then anss else (ans:ha4' f x anss))

ha4' :: Eq a => (Answer -> a) -> a -> [Answer] -> [Answer]
ha4' f x (a:anss) = if x == f a then anss else a:ha4' f x anss

{--
Generalized to the above

hasAll4Girls :: Clue -> Bool
hasAll4Girls = ha4g [Alberta .. Tamara] . Set.toList

ha4g :: [Girl] -> [Answer] -> Bool
ha4g [] [] = True
ha4g (_:_) [] = False
ha4g [] (_:_) = False
ha4g (g:gs) (a@(Ans g1 _ _):anss) = 
   ha4g gs (if g == g1 then anss else (a:ha4g' g anss))

ha4g' :: Girl -> [Answer] -> [Answer]
ha4g' g (a@(Ans g1 _ _):anss) = if g == g1 then anss else a:ha4g' g anss

hasAll4Birds :: Clue -> Bool
hasAll4Birds = ha4b [Canary .. Parrot] . Set.toList

ha4b :: [Bird] -> [Answer] -> Bool
ha4b [] [] = True
ha4b (_:_) [] = False
ha4b [] (_:_) = False
ha4b (b:bs) (a@(Ans _ b1 _):anss) =
   ha4b bs (if b == b1 then anss else (a:ha4b' b anss))

ha4b' :: Bird -> [Answer] -> [Answer]
ha4b' b (a@(Ans _ b1 _):anss) = if b == b1 then anss else a:ha4b' b anss
--}

solver :: [Clue] -> Set Clue
solver = Set.fromList
       . filter (\ans -> hasAll4 bird [Canary .. Parrot] ans
                      && hasAll4 girl [Alberta .. Tamara] ans
                      && hasAll4 month [January .. April] ans
                      && checkFebruary ans
                      && checkEllen ans)

checkEllen :: Set Answer -> Bool
checkEllen = ce . Set.toList

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)

ce :: [Answer] -> Bool
ce (a@(Ans e p m):anss) =
   if e == Ellen then (p == Canary) `xor` (m == February)
   else ce anss

checkFebruary :: Set Answer -> Bool
checkFebruary = cf . Set.toList

cf :: [Answer] -> Bool
cf (a@(Ans p b m):anss) =
   if m == February then (p == Ellen) `xor` (b == Finch)
   else cf anss

{--
>>> let ans = solver clues 
>>> length ans
1
>>> traverse (\set -> traverse print (Set.toList set) >> putStrLn "") (Set.toList ans)
Ans {girl = Alberta, bird = Finch, month = February}
Ans {girl = Ellen, bird = Canary, month = January}
Ans {girl = Ida, bird = Lorikeet, month = March}
Ans {girl = Tamara, bird = Parrot, month = April}

[()]
--}
