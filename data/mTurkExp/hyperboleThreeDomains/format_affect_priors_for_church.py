import sys, re, string

domain = sys.argv[2]
f = open(sys.argv[1], "r")

dictionary = dict()

SMALL_NUM = 0.00000001

firstline = 0
for l in f:
    if firstline == 0:
        firstline = 1
    else:
        if re.search(domain, l):
            l = l.strip()
            toks = l.split(", ")
            numberType = toks[7]
            if numberType == "round":
                utterance = int(toks[8])
            else:
                utterance = int(toks[8]) + 1
            value = toks[9]
            if value == "0":
                value = str(SMALL_NUM)
            if value == "1":
                value = str(1 - SMALL_NUM)
            if utterance in dictionary:
                dictionary[utterance].append(value)
            else:
                dictionary[utterance] = [value]

print "(list",
for k, v in sorted(dictionary.iteritems()):
    print "(list '" + str(k) + " (list",
    for prob in v:
        print "'" + prob,
    print "))",
print ")"
