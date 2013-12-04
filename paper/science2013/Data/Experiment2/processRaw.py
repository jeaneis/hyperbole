# trims down raw data file to include only relevant information
# removes and replaces excess symbols, punctuations, etc

import sys, re, string

f = open(sys.argv[1], "r")

# define the fields to keep
relevant_fields = ["workerid", "Answer.buyers", "Answer.gender", "Answer.domains", "Answer.utteredPricesRounded", "Answer.utteredTypes", "Answer.actualPricesRounded", "Answer.actualTypes", "Answer.affects", "Answer.nativeLanguage", "Answer.orders", "Answer.income","Answer.age", "Answer.sentenceIDs"]
relevant_indices = []

special_fields = ["Answer.utteredPrices", "Answer.actualPrices"]
special_indices = []
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
        for field in special_fields:
            special_indices.append(toks.index(field))
        firstline = 1
        print "\t".join(relevant_fields) + "\t" + "\t".join(special_fields)
    else:
        toks = l.split("\t")
        fieldsToPrint = []
        for i in relevant_indices:
            fieldToPrint = toks[i].replace('"', "")
            fieldsToPrint.append(fieldToPrint)
        for i in special_indices:
            fieldsToPrint.append(toks[i].replace('"",""', ";").replace(",","").replace(";", ",").replace('"', ""))
        print "\t".join(fieldsToPrint)

