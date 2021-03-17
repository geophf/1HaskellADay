import sys
import nltk

from nltk.stem.snowball import SnowballStemmer

file = open(sys.argv[1], 'r').read()
tokens = set(nltk.word_tokenize(file))
stemmer = SnowballStemmer("english")

print('token,stem')

for token in sorted(tokens):
   print(token + ',' + stemmer.stem(token))
