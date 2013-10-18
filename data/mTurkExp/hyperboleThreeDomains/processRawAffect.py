# trims down raw data file to include only relevant information
# removes and replaces excess symbols, punctuations, etc

import sys, re, string

f = open(sys.argv[1], "r")

# define the fields to keep
relevant_fields = ["workerid", "Answer.buyers", "Answer.gender", "Answer.affects", "Answer.nativeLanguage", "Answer.orders", "Answer.domains","Answer.income","Answer.age", "Answer.sentenceIDs", "Answer.utteredPricesRounded", "Answer.numberTypes"]
relevant_indices = []

firstline = 0

for l in f:
    l = l.strip()
    l = l.replace("[", "")
    l = l.replace("]", "")
    if firstline == 0:
        l = l.replace('"', "")
        toks = l.split("\t")
        for field in relevant_fields:
            relevant_indices.append(toks.index(field))
        firstline = 1
        print "\t".join(relevant_fields)
    else:
        toks = l.split("\t")
        fieldsToPrint = []
        for i in relevant_indices:
            fieldToPrint = toks[i].replace('"', "")
            fieldsToPrint.append(fieldToPrint)
        print "\t".join(fieldsToPrint)



