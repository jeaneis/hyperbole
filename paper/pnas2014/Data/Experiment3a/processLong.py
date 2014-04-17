import sys, re, string

f = open(sys.argv[1], "r")

firstline = 0

def normalize(vector):
    normalizer = sum(vector)
    return [x/normalizer for x in vector]


interpretations = [50, 51, 500, 501, 1000, 1001, 5000, 5001, 10000, 10001]

print "workerID,gender,language,income,age,domain,order,buyer,interpretation,interpretationType,interpretationRounded,interpretationProb"

for l in f:
    if firstline == 0:
        firstline = 1
    else:
        l = l.strip()
        toks = l.split(", ")
        responses = toks[6:16]
        responses = [float(x) for x in responses]
        responses = normalize(responses)
        for i in range(len(responses)):
            interpretation = str(interpretations[i])
            if float(interpretation)/10 == int(interpretation)/10:
                interpretationRounded = interpretation
                interpretationType = "round"
            else:
                interpretationRounded = str(int(interpretation) -1)
                interpretationType = "sharp"
            print ",".join(toks[0:6]) + "," + ",".join(toks[16:18]) + interpretation + "," + interpretationType + "," + interpretationRounded  + "," + str(responses[i])
