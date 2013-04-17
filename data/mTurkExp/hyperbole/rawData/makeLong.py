# turn trimmed data into long form

import sys, re, string

f = open(sys.argv[1], "r")

subject_fields = ["workerid", "Answer.gender", "Answer.nativeLanguage", "Answer.income", "Answer.age", "Answer.condition"]
item_fields = ["Answer.domains", "Answer.prices", "Answer.results", "Answer.orders", "Answer.buyers"]
numTrials = 96

# the order in which the fields are listed
print "workerID, gender, nativeLanguage, income, age, sentenceID, domain, numberType, utteredPrice, inferredPrice, probOpinion, order, buyer, "
subject_indices = [0,5,7,3,4]
item_indices = [8,2,10,12,6,1,9,11]
firstline = 0
for l in f:
    l = l.replace("\n", "")
    if firstline == 0:
        firstline = 1
    else:
        toks = l.split("\t")
        for n in range(numTrials):
            for s in subject_indices:
                print toks[s] + ",",
            for i in item_indices:
                vector = toks[i].split(",")
                print vector[n] + ",",
            print "\n",
