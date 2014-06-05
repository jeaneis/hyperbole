import sys, re, string

costs = [str(x/100.00) for x in range(11, 41)]
domains = ["electric kettle", "laptop", "watch"]
utterances = [50, 51, 500, 501, 1000, 1001, 5000, 5001, 10000, 10001]

for cost in costs:
    outputfilename = "ParsedOutput/output-" + cost + ".txt"
    wf = open(outputfilename, "w")
    wf.write("domain,utterance,state,affect,probability\n")
    for domain in domains:
        if domain == "electric kettle":
            name = "kettle"
        else:
            name = domain
        filename = "Output/hyperbole-" + name + "-" + cost + ".church"
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
                    wf.write(domain + "," + str(utterance) + "," + states[n].translate(None,"()").replace(" ", ",") + "," + probs[n] + "\n")
