"""
  COMS4115: Python Standard Library

  Authors:
    - Alex Kalicki
    - Alexandra Medway
    - Daniel Echikson
    - Lilly Wang
"""
import math
import random

INDEX_STEP = 1000
DIST_LENGTH = 10000

def make_dist(min, max, f):
    """Return a list generated from dist<min, max> | f"""
    step = (end - start) * 1.0 / INDEX_STEP
    indices = [ start + step * x for x in range(INDEX_STEP) ]

    cum_sum = 0.0
    cum_weights = []
    for x in indices:
        cum_sum += abs(f(x))
        cum_weights.append(cum_sum)
    rands = sorted([random.uniform(0, cum_sum) for x in range(DIST_LENGTH)])

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

"""
END PYTHON STANDARD LIBRARY
BEGIN USER CODE
"""
