"""
Keep the exact utterance, not just the representation of sharp or round.
"""
import sys, re, string

f = open(sys.argv[1], "r")

firstline = 0

def normalize(vector):
    normalizer = sum(vector)
    return [x/normalizer for x in vector]


interpretations = [500, 501, 2000, 2001, 10000, 10001]

print "workerID,gender,language,income,age,utteranceID,domain,utteranceType,utteranceRounded,exactUtterance,utterance,utteranceLength,opinionProb,order,interpretation,interpretationType,interpretationRounded,interpretationProb"

for l in f:
    if firstline == 0:
        firstline = 1
    else:
        l = l.strip()
        toks = l.split(", ")
        utteranceType = toks[7]
        utteranceRounded = int(toks[8])
        exactUtterance = toks[9].strip()
        if utteranceType == "sharp":
            utterance = str(utteranceRounded + 1)
        else:
            utterance = str(utteranceRounded)
        responses = toks[10:16]
        responses = [float(x)/40 for x in responses]
        responses = normalize(responses)
        utteranceLength = len(exactUtterance.split(" "))

        for i in range(len(responses)):
            interpretation = str(interpretations[i])
            if float(interpretation)/10 == int(interpretation)/10:
                interpretationRounded = interpretation
                interpretationType = "round"
            else:
                interpretationRounded = str(int(interpretation) -1)
                interpretationType = "sharp"
            print ",".join(toks[0:9]) + "," + exactUtterance +"," + utterance + "," + str(utteranceLength) + "," + ",".join(toks[16:18]) + "," + interpretation + "," + interpretationType + "," + interpretationRounded  + "," + str(responses[i])
