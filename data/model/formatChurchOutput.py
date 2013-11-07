import sys, re, string

utterances = [50, 51, 500, 501, 1000, 1001, 5000, 5001, 10000, 10001]

f = open(sys.argv[1], "r")

print "utterance,meaning,valence,probability"
for l in f:
    l = l.strip()
    l = l[3:len(l)-3]
    by_utterance = l.split(")) ((")
    for i in range(len(by_utterance)):
        utterance = utterances[i]
        output = by_utterance[i]
        states_probs = output.split(")) (")
        states = (states_probs[0] + ")").split(") (")
        probs = states_probs[1].split(" ")
        for n in range(len(states)):
            print str(utterance) + "," + states[n].translate(None,"()").replace(" ", ",") + "," + probs[n]
