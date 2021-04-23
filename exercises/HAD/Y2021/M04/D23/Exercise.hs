module Y2021.M04.D23.Exercise where

{--
There are so many ways Haskell and the 'real world' interface.

We've been looking at how (sometimes dirty) data intersects with Haskell-typed
values.

Here's an interesting 'real world'-problem.

Sometimes somebody factors data differently than you do. For example, wikidata's
geolocation as JSON is ... 'interesting.' So is neo4j's.

But that's why I have the LongLat-library. Ugh.

I recently came up with another example, and it can be stated thus:

The ability for a fighter to win a battle is based upon both Kung Fu 
fighting-style (and skill), and the foods eaten. These are two different
things, but they are both known as Skill
--}

data Skill = FS FightingStyle | FE FoodEaten
   deriving (Eq, Ord, Show)

data FightingStyle = Karate | KravMaga | TaeKwonDo | Savate | MuayThai | Arnis
   deriving (Eq, Ord, Show, Read)

-- https://medium.com/@krissturmey/the-diets-of-historys-greatest-warriors-4d7439c0b6bd

data FoodEaten = Fish | Meat | Barley | Rye | Oats | Milk | Eggs
   deriving (Eq, Ord, Show, Read)

-- and we have the complexity of Veggies and Fruits as multiple values
-- of a FoodEaten-variant, but I'm going to ignore that complexity for now.

{--
So, here's the thing: these skills are stored in two tables in a data-store,
but one table indicates kind (food eaten or fighting style), and the other
table has an admixture of both fighting-style and food-eaten values.

How do you read a value, given this unexpected factoring?
--}

readSkill :: String -> String -> Maybe Skill
readSkill fsfe kind = undefined

{--
readSkill takes a fighting-style or a food-eaten value-as-string, then it takes
the kind-as-string and returns a Skill-value, if possible.
--}

-- convert the below data into Skill values

warrior :: [(String, String)]
warrior =
   zip ["Krav Maga", "Barley", "Rye", "Milk", "Muay Thai"]
       ["Fighting Style", "Food Eaten", "Food Eaten", "Food Eaten", "Fighting Style"]

-- you'll also note that the data values are stored in manager-readable format
-- fix the values before parsing them.
