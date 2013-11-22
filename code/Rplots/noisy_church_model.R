noisy1 <- read.csv("~/Documents/Grad_school/Research/webchurch/test/HyperboleOutput/kettle_big_formatted.csv")
noisy2 <- read.csv("~/Documents/Grad_school/Research/webchurch/test/HyperboleOutput/laptop_big_formatted.csv")
noisy3 <- read.csv("~/Documents/Grad_school/Research/webchurch/test/HyperboleOutput/watch_big_formatted.csv")

noisy1$domain <- "electric kettle"
noisy2$domain <- "laptop"
noisy3$domain <- "watch"

noisy <- rbind(noisy1, noisy2, noisy3)

noisy$meaning <- factor(noisy$meaning)
noisy$utterance <- factor(noisy$utterance)
noisy$valence <- factor(noisy$valence)

noisy.justState <- aggregate(data=noisy, probability ~ domain + utterance + meaning + file, FUN=sum)


noisy.justState.summary <- summarySE(noisy.justState, measurevar="probability", 
                                     groupvars=c("utterance", "meaning", "domain"))

ggplot(noisy.justState.summary, aes(x=meaning, y=probability)) + 
  geom_bar(stat="identity", color="black", fill="gray") + 
  geom_errorbar(aes(ymin=probability-ci, ymax=probability+ci), width=0.2) +
  facet_grid(domain ~ utterance) +
  scale_x_discrete() +
  scale_y_continuous() +
  xlab("") +
  #ylab("") +
  theme_bw() +
  ylab("Probability") +
  xlab("Interpretation") +
  ggtitle("Model") +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=6, angle=-90),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=10))

noisyhuman.summary <- summarySE(d, measurevar="interpretationProb", 
                              groupvars=c("utterance", "interpretation", "domain"))
colnames(noisyhuman.summary)[2] <- "meaning"
noisy.comp <- join(noisyhuman.summary, noisy.justState.summary, by=c("utterance", "meaning", "domain"))

with(noisy.comp, cor.test(interpretationProb, probability))

##### Affect pairs #####

noisy.withAffect <- subset(noisy, valence=="1")
colnames(noisy.withAffect)[5] <- "affectProb"
noisy.noAffect <- subset(noisy, valence=="0")
colnames(noisy.noAffect)[5] <- "noAffectProb"

noisy.affect <- join(noisy.withAffect, noisy.noAffect, by=c("utterance", "meaning", "file", "domain"))
noisy.affect$affectRatio <- noisy.affect$affectProb / (noisy.affect$affectProb + noisy.affect$noAffectProb)

noisy.affect$utteranceRounded <- factor(floor(as.numeric(as.character(noisy.affect$utterance))/ 10)*10)
noisy.affect$meaningRounded <- factor(floor(as.numeric(as.character(noisy.affect$meaning))/ 10)*10)

noisy.hyperbole.affect <- subset(noisy.affect, as.numeric(as.character(noisy.affect$utteranceRounded)) >=
                          as.numeric(as.character(noisy.affect$meaningRounded)))

noisy.hyperbole.affect$isHyperbole <- 
  ifelse(noisy.hyperbole.affect$utteranceRounded==noisy.hyperbole.affect$meaningRounded, "literal", "hyperbole")


noisy.pair <- summarySE(noisy.hyperbole.affect, measurevar="affectRatio", 
                        groupvars=c("utteranceRounded", "meaningRounded", "isHyperbole", "domain"))

colnames(noisy.pair)[6] <- "modelAffect"

comp.noisy.pair <- join(human.pair, noisy.pair, 
                        by=c("utteranceRounded", "meaningRounded", "isHyperbole", "domain"))
comp.noisy.pair$label <- paste(comp.noisy.pair$utteranceRounded, comp.noisy.pair$meaningRounded, sep=",")

ggplot(comp.noisy.pair, aes(x=modelAffect, y=humanAffect)) +
  #geom_text(aes(label=label), color="dark grey") +
  geom_point(data=comp.noisy.pair, aes(x=modelAffect, y=humanAffect, color=isHyperbole, shape=domain), size=3) +
  geom_smooth(data=comp.noisy.pair, aes(x=modelAffect, y=humanAffect), method=lm, color="black", linetype=2) +
  geom_errorbar(aes(ymin=humanAffect-se, ymax=humanAffect+se), width=0.01, color="gray") +
  #geom_text(aes(label=utteranceRounded)) +
  theme_bw() +
  scale_color_brewer(palette="Set1") +
  xlab("Model") +
  ylab("Human") +
  theme(axis.title.x=element_text(size=16), axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        legend.title=element_text(size=0), legend.text=element_text(size=14),
        legend.position=c(0.9, 0.2))

# 0.7337842
with(comp.noisy.pair, cor.test(modelAffect, humanAffect))
