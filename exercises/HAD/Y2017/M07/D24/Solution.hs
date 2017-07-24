module Y2017.M07.D24.Solution where

{--
Okay, so, before we get back into the relational calculus via the Reasoned
Schemer by Byrd, et al, let's take a little commuter break this morning.

Whilst waiting at the bus stop, I observed the following about cars and their
passenger counts:

1 in 20 cars had 2 passengers (including the driver)
1 in 50 cars had 3 or more passengers.

How many cars out of 100 vehicles had only the driver?
--}

twoPassengers, threeOrMore, justTheDriver :: Rational
twoPassengers = 1 / 20
threeOrMore = 1 / 50
justTheDriver = 1 - twoPassengers - threeOrMore

{--
>>> twoPassengers 
1 % 20
>>> threeOrMore 
1 % 50
>>> justTheDriver 
93 % 100
--}

{--
Let's say that 1 in every 100 motor vehicles is public transport, you know:
a bus. Let's also say, and this is a gross assumption, but it works, that
a vehicle's emmission is about the same as any other vehicle's emmission on
the road.

Let's say that today 1 in 10 drivers decide to be weird today and don't, as
usual, commute into the exact same place that everybody else is going alone,
but instead, take the bus.

Okay, so, then, let's say on a normal day there are x vehicles on the road,
for those same number of vehicles, how many more people are commuting into work
today, given that 1 in 10 drivers decide to take the bus today.

Huh. Weird.
--}

normalDayPassengers, weirdDayPassengers :: Rational
normalDayPassengers =

-- okay, so from the above for 100 vehicles we have: x passengers which are
   
     (twoPassengers * 100) * 2
   + ((threeOrMore * 100) * 3) -- ... -ish.
   + (justTheDriver * 100)

{--
>>> normalDayPassengers 
109 % 1
--}

weirdDayPassengers =

{--
So, 109-ish people are on the road per 100 vehicles. We take 10 cars off the
road, remove those drivers to the bus, we have 90 vehicles on the road, but we
asked for 100 vehicles. What are the 10 additional vehicles that will go onto
the road to replace the 10 cars with just the drivers?

Will to be ten more busses? Nope:

>>> threeOrMore * 10
1 % 5

This says in 10 vehicles there will be 1/5 of a bus there (or 0.2 busses per
10 vehicles).

So, nope.

How about cars with two people in them?

>>> twoPassengers * 10
1 % 2

Nope. That says 1/2 car, or 0.5 car will be a two-person vehicle.

"But I wanna round up because I care about the environment and stuff!" you cry.

News flash: people driving to work don't. So, sorry, Charlie.

That means we have ten more driver-only cars on the road. Yay!
--}

   normalDayPassengers + 10

-- welp, that was easy, after a little bit of reasoning with a dash of
-- hard reality for ya.

-- The good news is that we have more workers going to work.
-- The bad news? Statistically: they are Gov't workers who actually do nothing
-- in staff jobs.
