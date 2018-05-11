module Y2018.M05.D11.Exercise where

{--
I don't know how this is going to play out, but that's what programming is:

Doing things you can do, and finding out what you 'can't' do, ... then doing it.

Today's Haskell problem: you're working in a mixed-language environment because
it's [the current year] now. Your scientist codes in Python and you code in
Haskell.

So, your scientist has given you a text-cleaning script, written in Python,
and you have a set of text manipulating functions, written in Haskell. You
need to deliver a system that is the integration of both those systems.

WAT DO?

Now, for this exercise you COULD rewrite the Python into Haskell and run the
one-language system that way, but there are situations where that is not
feasible. Pretend, for now, that today's exercise the feasible approach is
a ployglot solution.
--}

here :: FilePath -> FilePath
here = ("Y2018/M05/D11/" ++)

pythonModule :: FilePath
pythonModule = here "remove_accs.py"

wpjIntro :: FilePath
wpjIntro = here "wpj-intro.txt"

-- read in the World Policy Joural intro (wpjIntro) and apply the python
-- script alteration to the text to get the sanitized text. This may involve
-- using a FFI (foreign function interface) or Haskell-Python bridge-magic.

-- You figure it out.

updateTxt :: FilePath -> IO String
updateTxt art = undefined
