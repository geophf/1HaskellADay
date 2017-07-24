module Y2017.M07.D24.Exercise where

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
twoPassengers = undefined
threeOrMore = undefined
justTheDriver = undefined

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
normalDayPassengers = undefined
weirdDayPassengers = undefined

-- p.s.: yes, my specifications are imprecise. Welcome to industry. Deal with it
