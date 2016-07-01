module Data.SymbolTable where

-- TWO(!) solutions to the problem posted at http://lpaste.net/7204216668719939584

import Control.Monad
import Control.Monad.Trans.State  -- every monad is Trans these days?
import Data.Array
import Data.Maybe

-- is that discriminatory against cis-monads?
-- or are monads by definition in motion and therefore must be trans?
-- ('bind' is, after all, a transitive verb)

import Data.BidirectionalMap (BidirectionalMap)
import qualified Data.BidirectionalMap as BDM

{-- Okay, like I said: cat, splice-n-dice, many ways. MEOW!

But I'm going with a symbol-table here where we store strings-to-enumerated
values (ints) in a bidirectional map for easy toEnum retrievals. You'll see.

Now, to get the 'next' value we use State monad. --}

data SymbolTable = SymT { table :: BidirectionalMap String Int, top :: Int }
   deriving Show

-- our ground state:

empty :: SymbolTable
empty = SymT BDM.empty (negate 1) -- Haskell has to fix this, pronto!

{-- So what are we doing here?

instance Enum String where 
   toEnum num = undefined
   fromEnum str = undefined

Choose one of them. Make strings enumerable. Output the enumeration for the
following strings:

a, b, ab, The quick, brown fox jumped over the lazy dog.

Then 'de-enumerify' those value back to strings. --}

-- Okay, toEnum/fromEnum are occurring in the context or in the domain of
-- the SymbolTable State monad. This is problematic for the raw functions,
-- so let's just lift these unit functions into the State domain

strVal :: SymbolTable -> Int -> String
strVal syms = fromJust . (`BDM.lookback` (table syms))
   -- throws error if not there
   -- lookback function does a bi-directional map lookup from the value to key

toEnumS :: Monad m => Int -> StateT SymbolTable m String
toEnumS idx = liftM (`strVal` idx) get

intVal :: SymbolTable -> String -> Int
intVal syms = fromJust . (`BDM.lookup` (table syms))

fromEnumS :: Monad m => String -> StateT SymbolTable m Int
fromEnumS str = get >>= \(SymT table top) ->
   maybe (addSym str table (succ top)) return (BDM.lookup str table)

-- so ... the enumeration-typeclass is over State SymbolTable Int

-- adds a symbol to the symbol table if it's not there already

addSym :: Monad m => String -> BidirectionalMap String Int -> Int
                  -> StateT SymbolTable m Int
addSym str table n = put (SymT (BDM.insert str n table) n) >> return n

{-- So:

*Main> runState (fromEnumS "a") empty ~>
(0,SymT {table = BDMap fromList [("a",0)], top = 0})

and then ...

*Main> let status =
             foldr (\str state -> runState (fromEnumS str) (snd state))
                   (runState (fromEnumS "a") empty)
         ["a", "b", "ab", "The quick, brown fox jumped over the lazy dog."] ~>
(0,SymT {table = BDMap fromList [("a",0),("ab",2),("b",3),
                         ("The quick, brown fox jumped over the lazy dog.",1)]
         top = 3})

And given that:

*Main> evalState (toEnumS 0) $ snd status ~> "a"

Strings are now enumerable. To wit:

*Main> map (\x -> evalState (toEnumS x) $ snd status) [0 .. 3] ~>
["a","The quick, brown fox jumped over the lazy dog.","ab","b"]

And strings yield enumerated values:

*Main> map (\x -> evalState (fromEnumS x) $ snd status) it ~> [0,1,2,3]

TA-DAH! --}

{-- Epilogue

This is one way to do it. Another way, if you know all your strings statically,
is to compile the strings into an algebraic type in a separate module and making
that type showable and enumerable, e.g.:

data EnumedString = S0 | S1 | S2 | S3
   deriving (Eq, Ord, Enum, Bounded, Ix)

instance Show EnumedString where
  show S0 = "a"
  show S1 = "b"
  show S2 = "ab"
  show S3 = "The quick, brown fox jumped over the lazy dog."

since EnumedString is enumerable, fromEnum and toEnum work as expected, and
show gives you the string-representation you want. To wit:

*Main> S0 ~> a
*Main> S1 ~> b
*Main> S2 ~> ab
*Main> S3 ~> The quick, brown fox jumped over the lazy dog.
*Main> :t toEnum
toEnum :: Enum a => Int -> a
*Main> toEnum 3 :: EnumedString ~> The quick, brown fox jumped over the lazy dog.
*Main> fromEnum S2 ~> 2

This is using the haskell compiler to 'machine-encode' enumerated string. A wee-
bit more arcane, but automagic both in the String encoding and decoding (from/toEnum)
--}
