{-# LANGUAGE TupleSections #-}

module Data.Matrix where

{-- a solution to the problem posted at http://lpaste.net/3386266226073272320

Hey, Haskellers!

So 'yesterday'! We constructed a matrix from an array of arrays of things, but
is that really the way to go about doing things?

Why do you ask, geophf?

Thank you for that lead-in.

The thing is with arrays is that they are indexed by an Ix ('indexible')-type
and an indexible type is any type that can be used as an index (usually being an
Enum(erable)-type is a Good Thing(tm)).

Well, what's to stop our indexible-type from being a (2-)tuple of Ints?

What, indeed?

Why do you say all that, geophf?

Thank you. I say all that, because, in normal matrix-definitions (one of which
you saw 'yesterday') with Array-of-Arrays, indexing to a specific element 
requires a bit of tedium on our part to calculate which array and which element
of that array is the matrix-element we are looking for.

And I still.
Haven't found.
What I'm lookin' for.

BUT if we redeclare our ho-hum matrices to be SUPAH-MATRICES of one Array
indexed by a 2-tuple type, then finding the element we are looking for becomes
child's-play.

Let's do that.

Redeclare Matrix a to be an array-type indexed by a 2-tuple type. --}

import Control.Arrow
import Control.Lens
import Data.Array
import Data.List hiding (transpose)
import Data.Monoid
import Data.Tuple (swap)

import Control.Automata.Cellular (runRule, genRule, seed)
import Control.Logic.Frege ((-|), adjoin)
import Data.Stream (takeS)
import Data.Universe (toList)

data Matrix a = M { matrix :: Array (Int, Int) a } deriving (Eq, Show)
    -- the array has the bounds

dims :: Matrix a -> ((Int, Int), (Int, Int))
dims = bounds . matrix

-- define a (function-)type fromLists that takes a list-of-lists and returns
-- a matrix of those same dimensions. What happens if they lists are not all of
-- the same length?

fromLists :: [[a]] -> Matrix a
fromLists lists =
   M . array ((1,1), (n,m))
     . concat $ countWith (\row -> countWith (\col -> (,) (row,col))) lists
          where n = length lists
                m = length (head lists)

countWith :: (Int -> b -> c) -> [b] -> [c]
countWith f = zipWith f [1..]

-- redefine pprint so it works with this new type pretty-printing as before

pprint :: Show a => Matrix a -> IO ()
pprint (M arr) = -- pprintWith show
   let ((_,_),(rows,cols)) = bounds arr in
   putStrLn ("Matrix " ++ show rows ++ ('x':show cols)) >> showRows arr

{--
pprintWith :: (a -> String) -> Matrix a -> IO ()
pprintWith f (M arr) =
   let (rows,cols) = snd (bounds arr) in
   putStrLn ("Matrix " ++ show rows ++ ('x':show cols)) >> showRowsWith f arr
--}

maxWidth :: Show a => [a] -> Int
maxWidth = maxString . map show -- maximum $ map (succ . length . show) vals

maxString :: [String] -> Int
maxString = maximum . map (succ . length)

formatted :: Show a => Int -> a -> String
formatted padding val =
   let str = show val
       consumes = length str
   in  replicate (padding - consumes) ' ' ++ str

-- ya know: for right-justified, and stuff

showRow :: Show a => Int -> [a] -> IO ()
showRow padding vals =
   putStrLn ('|':concatMap (formatted padding) vals ++ " |")

showRows :: Show a => Array (Int,Int) a -> IO ()
showRows daterz =
   let cols = snd (snd (bounds daterz))
       elims = unfoldr (\lst -> let (lhs,rhs) = splitAt cols lst in
                                if null lhs then Nothing else Just (lhs,rhs)) 
                       (elems daterz)
       padding = maxWidth (concat elims)
   in  mapM_ (showRow padding) elims

{--
showRowsWith :: (a -> String) -> Array (Int,Int) a -> IO ()
showRowsWith f daterz =
   let cols  = snd (snd (bounds daterz))
       elims = unfoldr (\lst -> let (lhs,rhs) = splitAt cols lst in
                                if null lhs then Nothing else Just (lhs,rhs))
                       (elems daterz)
       padding = maxString (concatMap (map  f) elims)
   in  mapM_ (showRow padding) (concatMap (map f) elims)
--}

-- pprint the following matrix:

ourMatrix :: Matrix Int
ourMatrix = fromLists (take 5 (zipWith enumFromTo [1,6..] [5,10..]))

{--
*Data.Matrix> pprint ourMatrix 
Matrix 5x5
|  1  2  3  4  5 |
|  6  7  8  9 10 |
| 11 12 13 14 15 |
| 16 17 18 19 20 |
| 21 22 23 24 25 |
--}

-- What is the (3,4)-element of ourMatrix

threeDownLowOnThe4 :: (Int, Int) -> Matrix a -> a
threeDownLowOnThe4 idx = (! idx) . matrix

-- *Data.Matrix> threeDownLowOnThe4 (3,4) ourMatrix ~> 14

{--
So, we've now got a matrix that we can create and then view.

Very nice.

But what does one ... DO with it?

Well, the 'hello, world!' for matrices is the transpose-operation.

What is the transpose-operation, you ask.

Well, a good description of Data.Matrix.transpose can be found at 
Data.List.transpose. Okay read that.

You say: tl;dr.

Okay then:

transpose [[1,2,3]       == becomes ==>   [[1,4]
           [4,5,6]]                        [2,5]
                                           [3,6]]

Why is transpose important or useful, geophf?

My answer: wikipedia (aka: because same) https://en.wikipedia.org/wiki/Transpose

SO, now that we are forearmed with awesome knowledge, let us define transpose
for matrices.

Question: Data.List.transpose. Is it useful here, or not?
Question: I see a linear-time definition here. Do you?
--}

transpose :: Matrix a -> Matrix a
transpose =
   M . (array . adjoin swap . bounds <*> map (first swap) . assocs) . matrix

-- As Андреев Кирилл @nonaem00 for the #1Liner, swap is defined in Data.Tuple

-- what is transpose tehMatrix?

tehMatrix :: Matrix Int
tehMatrix = fromLists [[1,2,3],[4,5,6]]

-- I mean, like, show your haskelly-answer to that question, please.

{--
*Main> pprint $ transpose tehMatrix 
Matrix 3x2
| 1 4 |
| 2 5 |
| 3 6 |
--}

{--
Bonus question: 

I see a constant-time definition here from the wikipedia article. Do you?

(but that involves some 'magic' with 'views into' the (transposed-)matrix)

Oh, and besides element-access for matrices, which we ('kinda-really') defined
yesterday ... http://lpaste.net/3386266226073272320 we also want definitions
for row-views and column-views into matrices. Define those.
--}

cell :: Matrix a -> (Int, Int) -> a
cell (M arr) idx = arr ! idx

-- what is the (2,2)-cell of tehMatrix?
-- what is the (2,2)-cell of transpose tehMatrix?

{--
*Main> cell tehMatrix (2,2) ~> 5
*Main> cell (transpose tehMatrix) (2,2) ~> 5
--}

rows, cols :: Matrix a -> [[a]]
rows mat@(M arr) =
   let ((r1,_),(rn,_)) = bounds arr in map (flip row mat) [r1..rn]
cols mat@(M arr) = 
   let ((_,c1),(_,cn)) = bounds arr in map (flip col mat) [c1..cn]

-- what is rows tehMatrix?
-- what is cols tehMatrix?
-- what are they for transpose tehMatrix? What can be said about their relationship?

{--
*Main> rows tehMatrix ~> [[1,2,3],[4,5,6]]
*Main> cols tehMatrix ~> [[1,4],[2,5],[3,6]]

(rows &&& cols) tehMatrix == (cols &&& rows) (transpose tehMatrix)
--}

{-- A bit of lens-y exploration ...

-- now, defining the dual for lenses can be a ... bit more verbose:

dualL :: (Functor f, Field2 s t a b) => ((a -> f b) -> s -> f t)
                                     -> ((a -> f b) -> s -> f t)
dualL lens = view lens (_2, _1) -- #1Liner Michael Thomas @mjtjunior

vect :: Field2 s t a b => ((a -> Identity b) -> s -> Identity t)
                                    -> Int -> Matrix a -> [a]
vect tupf idx (M mat) =
   let range = (adjoin (view (dual tupf))) (bounds mat)
   in  map ((mat !) . flip (set (dual tupf) (set tupf idx (0,0))))
           (uncurry enumFromTo range)

-- #1Liner: how to make fst and snd tuple-setter functions
--}

-- what is row 2 for tehMatrix? ... for transpose tehMatrix?
-- what is col 2 for tehMatrix? ... for transpose tehMatrix?

{--
*Main> row 2 tehMatrix ~> [4,5,6]
*Main> row 2 $ transpose tehMatrix ~> [2,5]
*Main> col 2 tehMatrix ~> [2,5]
*Main> col 2 $ transpose tehMatrix ~> [4,5,6]
--}

-- hint: the definition for row and col functions may possible depend on the 
-- definitions for rows and cols functions.

-- (I didn't show a constant-time transpose. Any takers? ;)

{-- A solution to the problem posted at http://lpaste.net/2775082411233378304

Okay, so now that we've got row, col, and cell defined, what more could we
possibly want from matrices?

Multiplication, of course.

Okay, here we go.

Matrix-multiplication is defined as 

https://en.wikipedia.org/wiki/Matrix_multiplication

(Yes, a wikipedia article. Yes. I went there.)

Or, tl;dr    A(nxm) x B(mxp) = C(nxp) where each element in C is
defined as Cxy = (AxBy). (that is: the sum of the products of A's row x
and B's column y)

Let's look at the example where we want to buy beef, chicken and veggie-pies

https://www.mathsisfun.com/algebra/matrix-multiplying.html

If we have this matrix of pies that were sold

              Mon   Tues   Wed   Thur
Beef           13     9     7     15 
chicken         8     7     4      6
veggies         6     4     0      3

And their values are

Beef pie:    $3
Chicken pie: $4
Veggie pie:  $2

Then we can calculate how much we made using matrix multiplication:

[ 3 4 2 ]  x  (the matrix) = sales for each day.

Let's do this.

--}

cross :: Num a => Matrix a -> Matrix a -> Matrix a

-- welp, we could just do this declaratively: Crc = ArBc

cross a b =
   let range = \tupf -> adjoin tupf . dims
       (r1, rn) = range fst a
       rs = enumFromTo r1 rn
       (c1, cn) = range snd b
       cs = enumFromTo c1 cn
       bounds = ((r1,c1), (rn,cn))
   in  M (array bounds (rs >>= \r -> cs >>= \c ->
                        return ((r,c), dotProduct (row r a) (col c b))))

dotProduct :: Num a => [a] -> [a] -> a
-- unsafe check here?
dotProduct a = sum . zipWith (*) a

-- when you've defined the product function, multiply the above two matrices
-- (provided below). What is your result?

prices, sales :: Matrix Int
prices = fromLists [[3,4,2]]

sales = fromLists [[13,9,7,15],
                   [ 8,7,4, 6],
                   [ 6,4,0, 3]]

{-- Answer:

*Main> cross prices sales ~>
M {matrix = array ((1,1),(1,4)) [((1,1),83),((1,2),63),((1,3),37),((1,4),75)]}
*Main> pprint it
Matrix 1x4
| 83 63 37 75 |

--}

{--

A consideration

5 x 3 always works

(broad statement, but let's go with it)

But Ma x Mb doesn't always work, particularly if either is 
dimensionally-challenged ... how should we handle this type-wise?

Ma x Mb = Mabye Mc ???

So Matrix-multiplication is monadic then? Is that a Good Thing(tm)?

Or should 

Ma x Mb = error 

and we explode the universe because we attempted to multiply incompatible
matrices.

Thoughts? Discuss.
--}

{-- a solution to the problem posted at http://lpaste.net/7329174284021006336

So, today's problem is really a (silly) problem.

(silly because it's really silly how I defined row and col over-generally)

Look at the definition of row and col for Data.Matrix. What does it do?
(I renamed these functions to badRow and badCol, because accuracy)

Answer, obvs, is it returns the requested row or column of the matrix.

But how does these functions do this?

Answer, obvs: who cares? Haskell is a declarative language, and they do what
they are declared to do.

Right?

So, today's Haskell problem. Redefine row and col, not so that it scans the
entire matrix, but so that it returns the cells of that vector and considers
no other cells.

--}

row, col :: Int -> Matrix a -> [a]
row idx mat =
   map (cell mat . (idx,)) . uncurry enumFromTo . adjoin snd $ dims mat

{--
*Main> row 1 tehMatrix ~> [1,2,3]
*Main> row 2 tehMatrix ~> [4,5,6]
--}

col idx mat = -- same, but different
   map (cell mat . (,idx)) . uncurry enumFromTo . adjoin fst $ dims mat

{--
*Main> mapM_ (putStrLn . show) $ map (flip col tehMatrix) [1..3] 
[1,4]
[2,5]
[3,6]
--}

{-- Determining the determinant.

So, the determinant is for a square matrix, the + - + - ... of

Row1 cells * determinant of matrices made by all below cells not in that
row nor column. The matrices so formed are square. The determinant of a
unit matrix is the cell value.

Sounds recurrant.
--}

determinant :: Num a => Matrix a -> a
determinant = uncurry det . (id &&& rows)

det :: Num a => Matrix a -> [[a]] -> a
det _ [[ans]] = ans -- determinant of a 1x1 matrix is its sole element
det mat (row1:rows) =
   sum (zipWith3 product3 (cycle [1, -1]) row1
                   (map determinant (excludedSubMatrices mat 1)))

product3 :: Num a => a -> a -> a -> a
product3 a b c = a * b * c

excludedSubMatrices :: Matrix a -> Int -> [Matrix a]

-- setup for det: factors a matrix into matrices that are not in each of the
-- first row's columns

excludedSubMatrices mat row =
   let newbnd   = second (adjoin pred) (dims mat)
       lowmat   = filter ((/= row) . fst . fst) (assocs $ matrix mat)
       excludes = range (1, snd . snd $ dims mat)
   in  map (M . listArray newbnd . flip subMatrixOf lowmat) excludes

subMatrixOf :: Int -> [((Int, Int), a)] -> [a]
subMatrixOf excludeCol = map snd . filter ((/= excludeCol) . snd . fst)

-- With the above defined, what are the determinants of the below matrices?

ex1, ex2, ex10 :: Matrix Float

ex1  = fromLists [[2,5], [1, -3]]
ex2  = fromLists [[3,-5],[2,1]]
ex10 = fromLists [[2,-1,0], [3, -5, 2], [1,4,-2]]

-- exercises are from mathopolis.com

{--
*Data.Matrix> determinant ex1 ~> -11.0
*Data.Matrix> determinant ex2 ~> -13.0
*Data.Matrix> determinant ex10 ~> -4.0
*Data.Matrix> determinant (fromLists [[3,0,-1],[2,-5,4],[-3,1,3]]) ~> -44
*Data.Matrix> determinant (fromLists [[2,0,-1],[3,5,2],[-4,1,4]]) ~> 13
--}

{--
I'm sure there are many elegant ways to define the identity matrix.
I'll use Rule 16 from the rules of 2-dimensional cellular automata.
--}

identity :: Num a => Int -> Matrix a
identity n = fromLists (map (map (fromIntegral . fromEnum) . toList 0 n) cells)
   where cells = takeS n (runRule (genRule 16) seed)

{--
*Y2016.M06.D15.Solution> pprint (identity 5)
Matrix 5x5
| 1 0 0 0 0 |
| 0 1 0 0 0 |
| 0 0 1 0 0 |
| 0 0 0 1 0 |
| 0 0 0 0 1 |
Now show that the identity matrix is the identity matrix by crossing it with
ex1, for example. Generate the correctly sized identity matrix to cross with
any input matrix:

instance Monoid (Matrix a) where
   mempty             = error "No null-matrix defined"
   mempty `mappend` a = a
   a      `mappend` _ = a

sameMatrix :: (Eq a, Num a) => Matrix a -> Matrix a
sameMatrix mat =
   let (l,r) = snd (dims mat) in
   mat `cross` identity l == mat -| mat <> identity r `cross` mat == mat -| mat

*Y2016.M06.D15.Solution> pprint $ sameMatrix ex1
Matrix 2x2
|  2.0  5.0 |
|  1.0 -3.0 |
YAY!

The above (sameMatrix) is all very unit-testy; not sure if it should be here...

-- removing sameMatrix as the monoid definition gives an overlapping warning
--}
