d <- read.csv("../../data/mTurkExp/hyperboleOneDomainPilot/data4_exactWordUtterance_stretched.csv")
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
                             groupvars=c("exactUtterance", "utteranceType"))

ggplot(literal.summary, aes(x=exactUtterance, y=interpretationProb, fill=utteranceType)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  xlab("Utterred price") +
                  ylab("Probability of exact interpretation") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=18),
                        axis.text.x  = element_text(size=14, angle=90),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))

summary(lm(data=literal, interpretationProb ~ utteranceType * utteranceRounded))

# halo

counterpart1 <- subset(d, utteranceRounded==interpretationRounded)
counterpart1$halo <- ifelse(counterpart1$utterance == counterpart1$interpretation, "exact", "fuzzy")

counterpart1.summary <- summarySE(counterpart1, measurevar="interpretationProb",
                                  groupvars=c("exactUtterance", "halo"))

ggplot(counterpart1.summary, aes(x=exactUtterance, y=interpretationProb, fill=halo)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  xlab("Utterrance type") +
                  ylab("Probability") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=18),
                        axis.text.x  = element_text(size=14, angle=90),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10)) +
                          scale_fill_discrete(name="Interpretation type",
                                              breaks=c("exact", "fuzzy"),
                                              labels=c("Exact", "Fuzzy"))