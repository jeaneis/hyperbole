d <- read.csv("../../data/mTurkExp/hyperboleOneDomainPilot/data4_stretched.csv")
d$utteranceRounded <- factor(d$utteranceRounded)
d$utterance <- factor(d$utterance)
d$interpretationRounded <- factor(d$interpretationRounded)
d$interpretation <- factor(d$interpretation)

## join prior probability of prices
# read in price priors
p <- read.csv("../../data/mTurkExp/pricePriors/pricePriors_longForm.csv")
# get domain, make one data frame for utterance and one for interpretation
dom <- "laptop"
p.domain <- subset(p, domain==dom)

p.u.domain <- data.frame(utterance=factor(p.domain$meaning), utterancePriorProb=p.domain$logProb)
p.i.domain <- data.frame(interpretation=factor(p.domain$meaning), interpretationPriorProb=p.domain$logProb)

d <- join(d, p.u.domain, by="utterance")
d <- join(d, p.i.domain, by="interpretation")

# compare full interpretation distribution
full.summary <- summarySE(d, measurevar="interpretationProb",
                          groupvars=c("utterance", "interpretation"))

full.summary$interpretation <- factor(full.summary$interpretation)
ggplot(full.summary, aes(x=interpretation, y=interpretationProb)) +
  geom_bar(stat="identity", color="black", fill="#FF9999") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.2) +
  facet_grid(. ~ utterance) +
  theme_bw() +
  ylab("Probability") +
  xlab("Interpretation")

# get literal interpretations
literal <- subset(d, utterance==interpretation)
literal.summary <- summarySE(literal, measurevar="interpretationProb", 
                             groupvars=c("utteranceRounded", "utteranceType"))

ggplot(literal.summary, aes(x=utteranceRounded, y=interpretationProb, fill=utteranceType)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  xlab("Utterred price") +
                  ylab("Probability of exact interpretation") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=18),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))

summary(lm(data=literal, interpretationProb ~ utteranceType * utteranceRounded))

###### halo

# get interpretations for the counterpart (i.e. round goes to sharp and sharp goes to round)
counterpart <- subset(d, utteranceRounded==interpretationRounded & utteranceType != interpretationType)
counterpart.summary <- summarySE(counterpart, measurevar="interpretationProb",
                                 groupvars=c("utteranceRounded", "utteranceType", "utterance"))

ggplot(counterpart.summary, aes(x=utteranceRounded, y=interpretationProb, fill=utteranceType)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  xlab("Utterred price") +
                  ylab("Probability of counterpart interpretation") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=18),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))

summary(lm(data=counterpart, interpretationProb ~ utteranceType))

# get interpretations for the counterpart, 1 (i.e. goes to either itself or counterpart)
counterpart1 <- subset(d, utteranceRounded==interpretationRounded)
counterpart1$halo <- ifelse(counterpart1$utterance == counterpart1$interpretation, "exact", "fuzzy")

counterpart1.summary <- summarySE(counterpart1, measurevar="interpretationProb",
                                 groupvars=c("utteranceType", "halo"))

ggplot(counterpart1.summary, aes(x=utteranceType, y=interpretationProb, fill=halo)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  xlab("Utterrance type") +
                  ylab("Probability") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=18),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10)) +
                          scale_fill_discrete(name="Interpretation type",
                                              breaks=c("exact", "fuzzy"),
                                              labels=c("Exact", "Fuzzy"))

summary(lm(data=counterpart1, interpretationProb ~ utteranceType))
summary(lm(data=counterpart1, interpretationProb ~ halo))
summary(lm(data=counterpart1, interpretationProb ~ halo + utteranceType))

counterpart2.summary <- summarySE(counterpart1, measurevar="interpretationProb",
                                  groupvars=c("utteranceType", "utteranceRounded", "halo", "utterance"))

ggplot(counterpart2.summary, aes(x=utterance, y=interpretationProb, fill=halo)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  #facet_grid(.~utteranceRounded) +
                  xlab("Utterrance") +
                  ylab("Probability") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=18),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10)) +
                          scale_fill_discrete(name="Interpretation type",
                                              breaks=c("exact", "fuzzy"),
                                              labels=c("Exact", "Fuzzy"))


summary(lm(data=counterpart1, interpretationProb ~ utteranceRounded))
summary(lm(data=counterpart1, interpretationProb ~ utteranceRounded + utteranceType))
  

#### hyperbole effect
hyperbole <- subset(d, as.numeric(utteranceRounded)>as.numeric(interpretationRounded))
hyperbole.aggregate <- aggregate(interpretationProb ~ utterance + utteranceRounded + utteranceType + workerID, FUN=sum,
                                 data=hyperbole)

hyperbole.summary <- summarySE(hyperbole.aggregate, measurevar="interpretationProb",
                               groupvars=c("utteranceRounded", "utteranceType", "utterance"))

ggplot(hyperbole.summary.full, aes(x=utteranceRounded, y=interpretationProb, fill=utteranceType)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.2,
                position=position_dodge(0.9)) +
  theme_bw() +
  xlab("Utterrance") +
  ylab("Probability of hyperbole") +
  theme_bw() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=10))

# hyperbole and halo
# create a dummy data frame for utteranceRounded==500
hyperbole.dummy <- data.frame(utteranceRounded=factor(c(500, 500)), utteranceType=c("round", "sharp"),
                                                      utterance=factor(c(500,501)),
                                                      N=c(30,30), interpretationProb=c(0,0),
                                                      sd=c(0,0),se=c(0,0),ci=c(0,0))
hyperbole.summary.full <- rbind(hyperbole.dummy, hyperbole.summary)
hyperbole.summary.full$effect <- "hyperbole"
counterpart.summary$effect <- "halo"
hyperbole.summary.full <- rbind(hyperbole.summary.full, counterpart.summary)

ggplot(hyperbole.summary.full, aes(x=utterance, y=interpretationProb, group=effect, color=effect)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se), width=.1) +
  theme_bw()

halo.ratio <- literal
halo.ratio$fuzzyInterpretation <- counterpart$interpretationProb
halo.ratio$normalizer <- halo.ratio$fuzzyInterpretation + halo.ratio$interpretationProb
halo.ratio$fuzziness <- ifelse(halo.ratio$normalizer > 0, halo.ratio$fuzzyInterpretation / 
  (halo.ratio$interpretationProb + halo.ratio$fuzzyInterpretation), 0)

halo.ratio.summary <- summarySE(halo.ratio, measurevar="fuzziness",
                                groupvars=c("utteranceRounded", "utteranceType", "utterance"))


halo.ratio.summary$effect <- "halo"
colnames(halo.ratio.summary)[5] <- "interpretationProb"

ggplot(halo.ratio.summary, aes(x=utteranceRounded, y=interpretationProb, fill=utteranceType)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.2,
                position=position_dodge(0.9)) +
                  theme_bw()

hyperbole.summary.full <- rbind(hyperbole.summary.full, halo.ratio.summary)

ggplot(halo.ratio.summary, aes(x=utteranceRounded, y=interpretationProb, fill=utteranceType)) +
  geom_bar()

ggplot(hyperbole.summary.full, aes(x=utterance, y=interpretationProb, group=effect, color=effect)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se), width=.1) +
  theme_bw()

#### affect

affect <- unique(d[,1:12])
affect.summary <- summarySE(affect, measurevar="opinionProb",groupvars=c("utteranceType", 
                                                                         "utteranceRounded", "utterance"))

ggplot(affect.summary, aes(x=utteranceRounded, y=opinionProb, fill=utteranceType)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=opinionProb-se, ymax=opinionProb+se),width=0.2,
                position=position_dodge(0.9)) +
                  theme_bw() +
                  ylab("Probability of opinion") +
                  xlab("Utterance")


affect.verylikely <- subset(affect, utteranceRounded=="500")
summary(lm(data=affect.verylikely,opinionProb ~ utteranceType))
  