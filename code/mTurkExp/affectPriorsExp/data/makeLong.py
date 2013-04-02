# turn trimmed data into long form

import sys, re, string

f = open(sys.argv[1], "r")

subject_fields = ["workerid", "Answer.gender", "Answer.nativeLanguage", "Answer.income", "Answer.age", "Answer.condition"]
item_fields = ["Answer.domains", "Answer.prices", "Answer.results", "Answer.orders", "Answer.buyers"]
numTrials = 60

# the order in which the fields are listed
print "workerID, gender, nativeLanguage, income, age, condition, domain, price, result, order, buyer"
subject_indices = [0,2,4,7,9,3]
item_indices = [6,10,8,5,1]
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
                if i == 1:
                    print vector[n],
                else:
                    print vector[n] + ",",
            print "\n",
