import sys, re, string, random

domains = ["electric kettle", "watch", "laptop"]
numberTypes = ["round", "sharp"]
utterances = [50, 500, 1000, 5000, 10000]
buyers = ["Alex", "Bob", "Calvin", "David", "Eric", "Frank", "George", "Harry", "Ivan", "Jake", "Kenneth", "Luke", "Matt", "Nathan", "Owen",
        "Patrick", "Quinn", "Robert", "Steve", "Tom", "Victor", "Winston", "Zach", "Albert", "Barry", "Charles", "Daniel", "Ethan", "Fred", "Gary"]

inputs = []

sentenceID = 0
for domain in domains:
    if domain == "electric kettle":
        modifier = "an"
    else:
        modifier = "a"
    for numberType in numberTypes:
        for utterance in utterances:
            sentenceID += 1
            inputStr = '{"sentenceID":' + str(sentenceID) + ',"domain":"' + domain + '","modifier":"' + modifier + '","number":"' + numberType + '","utterance":' + str(utterance)            
            inputs.append(inputStr)

for n in range(len(inputs)):
        inputStr = inputs[n] + ',"condition":' + str(1) + ',"buyer":"' + buyers[n] + '"},'
        print inputStr

