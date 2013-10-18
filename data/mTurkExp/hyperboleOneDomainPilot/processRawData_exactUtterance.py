# trims down raw data file to include only relevant information
# removes and replaces excess symbols, punctuations, etc

import sys, re, string

f = open(sys.argv[1], "r")

# define the fields to keep
relevant_fields = ["workerid", "Answer.buyers", "Answer.gender", "Answer.affects", "Answer.nativeLanguage", "Answer.orders", "Answer.domains","Answer.income","Answer.inferredPrices0","Answer.inferredPrices1", "Answer.inferredPrices2", "Answer.inferredPrices3","Answer.inferredPrices4", "Answer.inferredPrices5", "Answer.age", "Answer.sentenceIDs", "Answer.numberTypes", "Answer.utteredPricesRounded" ]
relevant_indices = []
# field that needs to be specially processed
special_field = ["Answer.utteredPrices"]
special_index = []
firstline = 0

for l in f:
    l = l.replace("\n", "")
    l = l.replace("[", "")
    l = l.replace("]", "")
    if firstline == 0:
        l = l.replace('"', "")
        toks = l.split("\t") 
        for i in range(len(toks)):
            if toks[i] in relevant_fields:
                relevant_indices.append(i)
                print toks[i] + "\t",
            if toks[i] in special_field:
                special_index.append(i)
                print toks[i] + "\t",
        print "\n",
        firstline = 1
    else:
        toks = l.split("\t")
        for j in range(len(toks)):
            if j in relevant_indices:
                print toks[j].replace('"', "") + "\t",
            if j in special_index:
                special = toks[j].replace('"",""', ";").replace(",", "").replace(";", ",").replace('"', "")
                print special +"\t",
        print "\n",
