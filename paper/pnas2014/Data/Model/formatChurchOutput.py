import sys, re, string

domains = ["electric kettle", "laptop", "watch"]
utterances = [50, 51, 500, 501, 1000, 1001, 5000, 5001, 10000, 10001]

model = sys.argv[1]

print "domain,utterance,state,affect,probability"

for domain in domains:
    filename = model + "/" + domain.replace(" ", "-") + ".txt"
    f = open(filename, "r")
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
                print domain + "," + str(utterance) + "," + states[n].translate(None,"()").replace(" ", ",") + "," + probs[n]
