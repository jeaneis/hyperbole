# turn trimmed data into long form

import sys, re, string

f = open(sys.argv[1], "r")

subject_fields = ["workerid", "Answer.gender", "Answer.nativeLanguage", "Answer.income", "Answer.age"]
item_fields = ["Answer.sentenceIDs", "Answer.domains", "Answer.numberTypes", "Answer.utteredPricesRounded", "Answer.utteredPrices", "Answer.inferredPrices0", "Answer.inferredPrices1", "Answer.inferredPrices2", "Answer.inferredPrices3", "Answer.inferredPrices4", "Answer.inferredPrices5", "Answer.affects", "Answer.orders", "Answer.buyers"]
numTrials = 6
# the order in which the fields are listed
print "workerID, gender, nativeLanguage, income, age, sentenceID, domain, numberType, utteredPriceRounded, utteredPrice, inferredPrice0, inferredPrice1, inferredPrice2, inferredPrice3, inferredPrice4, inferredPrice5, probOpinion, order, buyer, "
subject_indices = []
item_indices = []
firstline = 0
for l in f:
    l = l.replace("\n", "")
    if firstline == 0:
        firstline = 1
        fields = l.split("\t")
        for subjectf in subject_fields:
            subject_indices.append(fields.index(subjectf))
        for itemf in item_fields:
            item_indices.append(fields.index(itemf))
        #print subject_indices
        #print item_indices
    else:
        toks = l.split("\t")
        for n in range(numTrials):
            for s in subject_indices:
                print toks[s] + ",",
            for i in item_indices:
                vector = toks[i].split(",")
                print vector[n] + ",",
            print "\n",
