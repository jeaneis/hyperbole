import sys, re, string, random

domains = ["electric kettle", "watch", "laptop"]
numberTypes = ["round", "sharp"]
utterances = [50, 500, 1000, 5000, 10000]

inputs = []

sentenceID = 0
for domain in domains:
    if domain == "electric kettle":
        modifier = "an"
    else:
        modifier = "a"
    for utteredNumberType in numberTypes:
        for uttered in utterances:
            for actualNumberType in numberTypes:
                for actual in utterances:
                    if uttered >= actual:
                        sentenceID += 1
                        inputStr = '{"sentenceID":' + str(sentenceID) + ',"domain":"' + domain + '","modifier":"' + modifier + '","utteredNumberType":"' + utteredNumberType + '","uttered":' + str(uttered) + ',"actualNumberType":"' + str(actualNumberType) + '","actual":' + str(actual)
                        inputs.append(inputStr)

for n in range(len(inputs)):
        inputStr = inputs[n] + '},'
        print inputStr
