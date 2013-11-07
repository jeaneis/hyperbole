import sys, re, string

f = open(sys.argv[1], "r")

firstline = 0
index = 0

def meanstdv(x):
    from math import sqrt
    n, mean, std = len(x), 0, 0
    for a in x:
        mean = mean + a
    mean = mean / float(n)
    for a in x:
        std = std + (a - mean)**2
    std = sqrt(std / float(n-1))
    return mean, std

def zscore(array):
    mean = meanstdv(array)[0]
    stdv = meanstdv(array)[1]
    zscored = [(i-mean)/stdv for i in array]
    return zscored


for l in f:
    l = l.strip()
    if firstline == 0:
        toks = l.split("\t")
        index = toks.index("Answer.affects")
        print l
        firstline = 1
    else:
        toks = l.split("\t")
        affects = [float(i) for i in toks[index].split(",")]
        zscored_affects = [str(i) for i in zscore(affects)]
        print "\t".join(toks[0:index]) + "\t" + ",".join(zscored_affects) + "\t" + "\t".join(toks[index+1:])


