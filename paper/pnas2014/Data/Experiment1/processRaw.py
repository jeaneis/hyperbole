# trims down raw data file to include only relevant information
# removes and replaces excess symbols, punctuations, etc

import sys, re, string

f = open(sys.argv[1], "r")

# define the fields to keep
relevant_fields = ["workerid", "Answer.buyers", "Answer.gender", "Answer.affects", "Answer.nativeLanguage", "Answer.orders", "Answer.domains","Answer.income","Answer.inferredPrices0","Answer.inferredPrices1", "Answer.inferredPrices2", "Answer.inferredPrices3","Answer.inferredPrices4", "Answer.inferredPrices5", "Answer.inferredPrices6", "Answer.inferredPrices7", "Answer.inferredPrices8", "Answer.inferredPrices9","Answer.age", "Answer.sentenceIDs", "Answer.numberTypes", "Answer.utteredPricesRounded" ]
relevant_indices = []

#field that needs to be specially processed
special_field = ["Answer.utteredPrices"]
special_index = 0
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
        special_index = toks.index(special_field[0])
        firstline = 1
        print "\t".join(relevant_fields) + "\t" + "\t".join(special_field)
    else:
        toks = l.split("\t")
        fieldsToPrint = []
        for i in relevant_indices:
            fieldToPrint = toks[i].replace('"', "")
            fieldsToPrint.append(fieldToPrint)
        fieldsToPrint.append(toks[special_index].replace('"",""', ";").replace(",", "").replace(";", ",").replace('"', ""))
        print "\t".join(fieldsToPrint)



