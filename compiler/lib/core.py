"""
  COMS4115: A compiled Odds program.

  Authors:
    - Alex Kalicki
    - Alexandra Medway
    - Daniel Echikson
    - Lilly Wang
"""
from __future__ import print_function
import math
import random
import sys

# Odds constants
EUL = math.e
PI = math.pi

INDEX_STEP = 1000
DIST_LENGTH = 10000
SAMPLE_STEP = 100

def exception(s):
    """Write exception s to stderr and exit program"""
    sys.stderr.write("%s\n" % s)
    exit(1)

def print(*args, **kwargs):
    """Call normal print() function, but return argument that was passed"""
    __builtins__.print(*args, **kwargs)
    return str(args[0])

def make_dist(start, end, f):
    """Return a list generated from dist<min, max> | f"""
    step = (end - start) * 1.0 / INDEX_STEP
    indices = [ start + step * x for x in range(INDEX_STEP) ]

    cum_sum = 0.0
    cum_weights = []
    for x in indices:
        cum_sum += abs(f(x))
        cum_weights.append(cum_sum)
    rands = sorted([ random.uniform(0, cum_sum) for x in range(DIST_LENGTH) ])

    cum_i = 0
    rand_i = 0
    dist_list = []
    while rand_i < len(rands):
        if rands[rand_i] < cum_weights[cum_i]:
            dist_list.append(indices[cum_i])
            rand_i = rand_i + 1
        else:
            cum_i = cum_i + 1
    return dist_list

def add_dist(d1, d2):
    """Return the sum of two distributions, adding each combination"""
    s1 = d1[random.randint(0, SAMPLE_STEP - 1)::SAMPLE_STEP]
    s2 = d2[random.randint(0, SAMPLE_STEP - 1)::SAMPLE_STEP]
    return sorted([ x + y for x in s1 for y in s2 ])

def mult_dist(d1, d2):
    """Return the product of two distributions, multiplying each combination"""
    s1 = d1[random.randint(0, SAMPLE_STEP - 1)::SAMPLE_STEP]
    s2 = d2[random.randint(0, SAMPLE_STEP - 1)::SAMPLE_STEP]
    return sorted([ x * y for x in s1 for y in s2 ])

def shift_dist(n, d):
    """Shift each element in distribution d by n"""
    return [ x + n for x in d ]

def stretch_dist(n, d):
    """Stretch distribution d, multiplying each element by n"""
    return [ x * n for x in d ]

def exp_dist(n, d):
    """Exponentiate distribution d, raising each element to power n"""
    return [ x ** n for x in d ]

def sample_dist(n, d):
    """Return a random sample of n elements in distribution d"""
    return sorted([ random.randint(0, DIST_LENGTH - 1) for x in range(n) ])

def prob(d, n):
    return len([i for i in d if i < n]) / DIST_LENGTH

def expected(d):
    return sum(d) / DIST_LENGTH

def concat_str(s1, s2):
    return s1 + s2

"""
END ODDS CORE LIBRARY
BEGIN USER CODE
"""
