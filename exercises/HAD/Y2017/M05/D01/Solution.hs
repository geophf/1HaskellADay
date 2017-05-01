module Y2017.M05.D01.Solution where

import Control.Monad (guard)

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege ((-|))

{--
From the Mensa Genius Quiz-a-Day book by Dr. Abbie F. Salny, etc.

It's hard to keep within a budget, ...

... [let that sink in] ...

Maura discovered as she prepared her class's May Day party. She had been 
assigned money from the common fund, and she spent half of it plus $2.00 for a
nice cake. Then she spent half of what she had left plus $2.00 for a baskets and
flowers. Then she spent half of what she had left plus $1.00 for candy. At that
point she was out of money. How much had she started with?

Side question: what was hard about keeping within the budget if, as this problem
states, Maura kept within the budget?

Assumption: she spent whole dollars on each purchase.
--}

type Funds = Int

costofcake, basketsFlowers, candy :: (Integral a, Monad m, Monoid (m a)) =>
      a -> m a
costofcake funds = even funds -| return (funds `div` 2 + 2)
basketsFlowers funds = even funds -| return (funds `div` 2 + 2)
candy funds = even funds -| return (funds `div` 2 + 1)

-- Side question: was the cake a lie? Did Maura eat the cake she bought?
-- was it delicious? Was there a candle, or no?

-- given the above, what was Maura's budget?

budget :: [Funds]
budget = [2..] >>= \allotted -> 
   costofcake allotted >>= \cake ->
   let remaining = allotted - cake in
   guard (remaining > 0) >>
   basketsFlowers remaining >>= \bf ->
   let newsum = remaining - bf in
   guard (newsum > 0) >>
   candy newsum >>= guard . (== newsum) >> return allotted

{--
>>> head budget 
20

(solution set diverges after the first (only) solution.

That's one way to do it. The other way is to work from the solution backwards.

We know that Maura spends all the money when she purchases the candy, so we
run the equations backwards:
--}

backwards :: Funds
backwards =
   let precandy = 1 * 2 
 -- (candy = funds / 2 + 1, and since money after candy ==0, funds = 1 * 2
       prebasketsFlowers = (precandy + 2) * 2
       precake = (prebasketsFlowers + 2) * 2
   in  precake

{--
>>> backwards 
20

So we have two ways of going about solving this problem.
--}
