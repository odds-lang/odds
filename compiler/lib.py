"""
  COMS4115: Semantic Analyzer

  Authors:
    - Alex Kalicki
    - Alexandra Medway
    - Daniel Echikson
    - Lilly Wang
"""

import random

INDEX_STEP = 1000
DIST_LENGTH = 10000

def make_dist(start, end, f):
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
    print rands[0]
    print cum_weights[0]
    while rand_i < len(rands):
        if rands[rand_i] < cum_weights[cum_i]:
            dist_list.append(indices[cum_i])
            rand_i = rand_i + 1
        else:
            cum_i = cum_i + 1
    return dist_list
