import sys, re, string

domains = ["kettle", "laptop", "watch"]
costs = [str(x/100.0) for x in range(11, 41)]

for domain in domains:
    filename = "hyperbole-" + domain + ".church"
    for cost in costs:
        outputfilename = "hyperbole-" + domain + "-" + cost + ".church"
        f = open(filename, "r")
        wf = open("WithUtteranceCosts/" + outputfilename, "w")
        wf.write("(define (utterance-prior) (multinomial utterances '(" + cost + " 0.1 " + cost + " 0.1 " + cost + " 0.1 " + cost + " 0.1 " + cost + " 0.1)))\n")
        for l in f:
            wf.write(l)


    

