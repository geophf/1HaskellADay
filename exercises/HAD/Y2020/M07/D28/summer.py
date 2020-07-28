#!/usr/local/bin/python

# sums the first n numbers, but if n > 100, returns an error

import sys

n = int(sys.argv[1])

if n > 100:
   sys.exit(1)

print(int(n * (n - 1) / 2))
