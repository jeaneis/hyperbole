import sys, re, string

f = open(sys.argv[1], "r")

firstline = 0

for l in f:
    l = l.replace("\n", "")
    if firstline == 0:
        firstline = 1
    else:
        toks = l.split(",")
        print '{"sentenceID":' + toks[0] + ',"domain":"' + toks[1] + '","modifier":"' + toks[2] + '","number":"' + toks[3] + '","utterance":' + toks[4] + ',"buyer":"' + toks[5] + '"},'
