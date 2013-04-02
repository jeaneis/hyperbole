import re, string, sys

f = open("../data/prices_rounded.csv")

names = []
firstline = 0
for l in f:
    l = l.replace("\n", "")
    if firstline == 0:
        names = l.split(",")
        firstline = 1
    else:
        toks = l.split(",")
        print '{"condition":' + toks[0] + ',"modifier":"' + toks[1] + '","domain":"' + toks[2] + '","price":' + toks[3] + ',"buyer":"' + toks[4] + '"},' 
