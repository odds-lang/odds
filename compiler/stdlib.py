def make_dist(min, max, f):
    step = (max - min) / 1000
    cutoff = [f((min + x * float((max - min)) / 1000)) for x in range(1, 1001)]
    total = sum(cutoff)
    dist = []
    for i in cutoff:
        num = float(i * 10000/ total)
        for j in range(0, int(round(num))):
            dist.append(i)
    for i in range(0, 10000 - len(dist)):
        dist.append(cutoff[len(cutoff)-1])
    return dist
