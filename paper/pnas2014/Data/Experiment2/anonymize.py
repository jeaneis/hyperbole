import sys, re, string

f = open(sys.argv[1], "r")

workerDict = dict()

ID = 0
firstline = 0
for l in f:
    l = l.strip()
    if firstline == 0:
        print l
        firstline = 1
    else:
        toks = l.split(",")
        workerID = toks[0]
        if workerID not in workerDict:
            ID = ID + 1
            workerDict[workerID] = ID
        print str(ID) + "," + ",".join(toks[1:])

