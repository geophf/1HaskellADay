module Y2018.M11.D26.Exercise where

{--
An IQ puzzler:

Four (A, B, C and D) suspects were interrogated:
A said: C won't cheat unless B cheated.
B said: Either A or B cheated.
C said: B didn't cheat, I cheated.
D said: B cheated.

Only one person is lying. Who is lying and who(st(s)) cheated?
--}

data Suspect = A | B | C | D
   deriving (Eq, Ord, Enum, Show)

type Cheated = Bool
type Liar = Bool

data Statement = AsYouDeclare

statements :: [Statement]
statements = undefined

data Answer = Ans { liar :: Suspect, cheaters :: [Suspect] }
   deriving (Eq, Ord, Show)

solver :: [Statement] -> [Answer]
solver statements = undefined


