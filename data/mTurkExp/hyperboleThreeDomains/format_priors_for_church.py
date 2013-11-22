import sys, re, string
# specify a domain to extract
domain = sys.argv[2]
f = open(sys.argv[1], "r")
firstline = 0

dictionary = dict()
for l in f:
    if firstline == 0:
        firstline = 1
    else:
        l = l.strip()
        if re.search(domain, l):
            toks = l.split(",")
            turkID = toks[0]
            value = toks[11]
            if value == "0.0":
                value = "0.0000000001"
            if turkID in dictionary:
                dictionary[turkID].append(value)
                #values = values.append(value)
                #dictionary[turkID] = values
            else:
                dictionary[turkID] = [value]

for k, v in dictionary.iteritems():
    print "'(" + " ".join(v) + ")",


