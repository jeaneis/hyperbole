# trims down raw data file to include only relevant information
# removes and replaces excess symbols, punctuations, etc

import sys, re, string

f = open(sys.argv[1], "r")

# define the fields to keep
relevant_fields = ["workerid", "Answer.buyers", "Answer.gender", "Answer.condition", "Answer.nativeLanguage", "Answer.orders", "Answer.domains","Answer.income","Answer.results", "Answer.age", "Answer.prices"]
relevant_indices = []

firstline = 0
for l in f:
    l = l.replace("\n", "")
    l = l.replace('"', "")
    l = l.replace("[", "")
    l = l.replace("]", "")
    toks = l.split("\t")
    if firstline == 0:
        for i in range(len(toks)):
            if toks[i] in relevant_fields:
                relevant_indices.append(i)
                print toks[i] + "\t",
        print "\n",
        firstline = 1
    else:
        for j in range(len(toks)):
            if j in relevant_indices:
                print toks[j] + "\t",
        print "\n",
