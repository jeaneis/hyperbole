import sys, re, string

f = open(sys.argv[1], "r")

firstline = 0

def normalize(vector):
    normalizer = sum(vector)
    return [x/normalizer for x in vector]


interpretations = [500, 501, 2000, 2001, 10000, 10001]

print "workerID,gender,language,income,age,utteranceID,domain,utteranceType,utteranceRounded,utterance,opinionProb,order,interpretation,interpretationType,interpretationRounded,interpretationProb"

for l in f:
    if firstline == 0:
        firstline = 1
    else:
        l = l.strip()
        toks = l.split(", ")
        utteranceType = toks[7]
        utteranceRounded = int(toks[8])
        if utteranceType == "sharp":
            utterance = str(utteranceRounded + 1)
        else:
            utterance = str(utteranceRounded)
        responses = toks[9:15]
        responses = [float(x)/40 for x in responses]
        responses = normalize(responses)
        for i in range(len(responses)):
            interpretation = str(interpretations[i])
            if float(interpretation)/10 == int(interpretation)/10:
                interpretationRounded = interpretation
                interpretationType = "round"
            else:
                interpretationRounded = str(int(interpretation) -1)
                interpretationType = "sharp"
            print ",".join(toks[0:9]) + "," + utterance + "," + ",".join(toks[15:17]) + "," + interpretation + "," + interpretationType + "," + interpretationRounded  + "," + str(responses[i])
