##########################
# Visualize human output
##########################

#d <- read.csv("../../data/mTurkExp/hyperboleOneDomainPilot/data2_stretched_exactUtterance.csv")
d <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/data1017_normalized.csv")
d$utteranceRounded <- factor(d$utteranceRounded)
d$utterance <- factor(d$utterance)
d$interpretationRounded <- factor(d$interpretationRounded)
d$interpretation <- factor(d$interpretation)

# ## join prior probability of prices
# # read in price priors
# p <- read.csv("../../data/mTurkExp/pricePriors/pricePriors_longForm.csv")
# # get domain, make one data frame for utterance and one for interpretation
# dom <- "laptop"
# p.domain <- subset(p, domain==dom)
# 
# p.u.domain <- data.frame(utterance=factor(p.domain$meaning), utterancePriorProb=p.domain$logProb)
# p.i.domain <- data.frame(interpretation=factor(p.domain$meaning), interpretationPriorProb=p.domain$logProb)
# 
# d <- join(d, p.u.domain, by="utterance")
# d <- join(d, p.i.domain, by="interpretation")

# compare full interpretation distribution

  full.summary <- summarySE(d, measurevar="interpretationProb",
                            groupvars=c("utterance", "interpretation", "domain"))
  
  full.summary$interpretation <- factor(full.summary$interpretation)
  human.full.plot <- ggplot(full.summary, aes(x=interpretation, y=interpretationProb)) +
    geom_bar(stat="identity", color="black", fill="#FF9999") +
    geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.2) +
    facet_grid(domain ~ utterance) +
    theme_bw() +
    ylab("Probability") +
    xlab("Interpretation") +
    theme(axis.title.x = element_text(size=12),
          axis.text.x  = element_text(size=10, angle=-90),
          axis.title.y = element_text(size=12),
          axis.text.y = element_text(size=10))


# get literal interpretations
literal <- subset(d, utterance==interpretation)
literal.summary <- summarySE(literal, measurevar="interpretationProb", 
                             groupvars=c("utteranceRounded", "utteranceType", "domain"))

ggplot(literal.summary, aes(x=utteranceRounded, y=interpretationProb, fill=utteranceType)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  facet_grid(domain ~.) +
                  xlab("Utterred price") +
                  ylab("Probability of exact interpretation") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=18),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))

summary(lm(data=literal, interpretationProb ~ utteranceType * utteranceRounded))

###### halo

# get interpretations for the counterpart, 1 (i.e. goes to either itself or counterpart)
counterpart <- subset(d, utteranceRounded==interpretationRounded)
counterpart$halo <- ifelse(counterpart$utterance == counterpart$interpretation, "exact", "fuzzy")

# visualize fuzzy vs exact interpretation
counterpart.summary <- summarySE(counterpart, measurevar="interpretationProb",
                                 groupvars=c("utteranceType", "halo", "domain"))
ggplot(counterpart.summary, aes(x=utteranceType, y=interpretationProb, fill=halo)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  facet_grid(domain ~.) +
                  xlab("Utterrance type") +
                  ylab("Probability") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=14),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))

# get interpretation probs for non-hyperbole (i.e., sum of going to itself or counterpart)
nonhyperbole <- aggregate(data=counterpart, interpretationProb ~ workerID + domain + utterance, FUN=sum)
colnames(nonhyperbole) <- c("workerID", "domain", "utterance", "nonhyperboleProb")

# join the two probs to get proportion of exact/fuzzy given non hyperbole interpretation
counterpart.nonhyperbole <- join(counterpart, nonhyperbole, by=c("workerID", "domain", "utterance"))
counterpart.nonhyperbole$haloProb <- 
  ifelse(counterpart.nonhyperbole$nonhyperboleProb > 0, 
         counterpart.nonhyperbole$interpretationProb/counterpart.nonhyperbole$nonhyperboleProb,
         0)


# get p(fuzzy | non-hyperbole)
halo <- subset(counterpart.nonhyperbole, halo=="fuzzy")

halo.summary <- summarySE(halo, measurevar="haloProb",
                                  groupvars=c("utteranceType", "domain"))

ggplot(halo.summary, aes(x=utteranceType, y=haloProb, fill=utteranceType)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=haloProb-se, ymax=haloProb+se),width=.2,
                position=position_dodge(.9)) +
                  facet_grid(domain ~.) +
                  xlab("Utterrance type") +
                  ylab("Probability") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=14),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))

# hyperbole

hyperbole <- subset(d, as.numeric(utteranceRounded) > as.numeric(interpretationRounded))
hyperbole.agg <- aggregate(data=hyperbole, interpretationProb ~ workerID + domain + utteranceType + utteranceRounded, FUN=sum)
hyperbole.summary <- summarySE(hyperbole.agg, measurevar="interpretationProb", 
                               groupvars=c("utteranceType", "domain", "utteranceRounded"))

ggplot(hyperbole.summary, aes(x=utteranceRounded, y=interpretationProb, fill=utteranceType)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  facet_grid(domain ~.) +
                  xlab("Utterrance rounded") +
                  ylab("Probability") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=14),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))

hyperbole.acrossUtterance.summary <- summarySE(hyperbole.agg, measurevar="interpretationProb", 
                                               groupvars=c("utteranceType", "domain"))
ggplot(hyperbole.acrossUtterance.summary, aes(x=utteranceType, y=interpretationProb, fill=utteranceType)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  facet_grid(domain ~.) +
                  xlab("Utterrance rounded") +
                  ylab("Probability") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=14),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))


# opinion

opinion.summary <- summarySE(d, measurevar="opinionProb", groupvars=c("domain", "utteranceType", "utteranceRounded"))
ggplot(opinion.summary, aes(x=utteranceRounded,y=opinionProb,fill=utteranceType)) +
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  geom_errorbar(aes(ymin=opinionProb-se, ymax=opinionProb+se),width=.2,
                position=position_dodge(.9)) +
                  facet_grid(domain ~.) +
                  xlab("Utterrance rounded") +
                  ylab("Probability") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=14),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))

# four kinds of information:
# exact, fuzzy, hyperbolic, opinion

# mark each interpretation as exact, fuzzy, or hyperbolic
d$interpretationKind <- 
  ifelse(as.numeric(d$utteranceRounded) > as.numeric(d$interpretationRounded), "hyperbolic", 
      ifelse(d$utterance==d$interpretation, "exact",
             ifelse(d$utteranceRounded==d$interpretationRounded, "fuzzy", "other")))

interpretation.agg <- aggregate(data=d, interpretationProb ~ 
  workerID + domain + utterance + interpretationKind, FUN=sum)
interpretation.summary <- summarySE(interpretation.agg, measurevar="interpretationProb",
                                    groupvars=c("utterance", "domain", "interpretationKind"), .drop=FALSE)

opinion.agg <- aggregate(data=d, opinionProb ~ workerID + domain + utterance, FUN=mean)
opinion.agg.summary <- summarySE(opinion.agg, measurevar="opinionProb", groupvars=c("utterance", "domain"))
opinion.agg.summary$interpretationKind <- "expensive"
colnames(opinion.agg.summary)[4] <- "interpretationProb"

interpretation.summary.clean <- rbind(
  subset(interpretation.summary, interpretationKind != "other"), opinion.agg.summary)

interpretation.summary.clean$interpretationKind <- 
  factor(interpretation.summary.clean$interpretationKind, levels=c("exact", "fuzzy", "hyperbolic", "expensive"))

ggplot(interpretation.summary.clean, aes(x=utterance, y=interpretationProb, fill=interpretationKind)) +
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  facet_grid(interpretationKind ~ domain) +
                  xlab("Utterance") +
                  ylab("Probability") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=14),
                        axis.text.x  = element_text(size=10, angle=-90),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))

# full exactness analysis
d.exact <- subset(d, utterance==interpretation)
summary(lm(data=d.exact, interpretationProb ~ utteranceRounded))
summary(lm(data=d.exact, interpretationProb ~ utteranceType))
summary(lm(data=d.exact, interpretationProb ~ utteranceRounded + utteranceType + domain))
summary(lm(data=d.exact, interpretationProb ~ utteranceRounded * utteranceType * domain))

d.hyperbole <- subset(d, as.numeric(utteranceRounded) > as.numeric(interpretationRounded))
d.hyperbole.agg <- aggregate(data=d.hyperbole,
                             interpretationProb ~ workerID + domain + utteranceRounded + utteranceType, FUN=sum)
summary(lm(data=d.hyperbole.agg, interpretationProb ~ utteranceRounded))
summary(lm(data=d.hyperbole.agg, interpretationProb ~ utteranceType))
summary(lm(data=d.hyperbole.agg, interpretationProb ~ utteranceRounded + utteranceType))

## just look at laptop (or any one domain)
d.laptop <- subset(d, domain=="electric kettle")
d.laptop.summary <- summarySE(d.laptop, measurevar="interpretationProb",
                          groupvars=c("utterance", "interpretation"))

d.laptop.summary$interpretation <- factor(d.laptop.summary$interpretation)

d.plot <- ggplot(d.laptop.summary, aes(x=interpretation, y=interpretationProb)) +
  geom_bar(stat="identity", color="black", fill="#FF9999") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.2) +
  facet_grid(.~ utterance) +
  theme_bw() +
  ylab("Probability") +
  xlab("Interpretation") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=10, angle=-90),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=10))

# exact interps
d.laptop.exact <- subset(d.laptop, utterance==interpretation)
summary(lm(data=d.laptop.exact, interpretationProb ~ utteranceRounded))
summary(lm(data=d.laptop.exact, interpretationProb ~ utteranceType))
summary(lm(data=d.laptop.exact, interpretationProb ~ utteranceRounded + utteranceType))
summary(lm(data=d.laptop.exact, interpretationProb ~ utteranceRounded * utteranceType))
# hyperbolic interps
d.laptop.hyperbole <- subset(d.laptop, as.numeric(utteranceRounded)>as.numeric(interpretationRounded))
d.laptop.hyperbole.agg <- aggregate(data=d.laptop.hyperbole,
                                    interpretationProb ~ workerID + utteranceRounded + utteranceType, FUN=sum)
summary(lm(data=d.laptop.hyperbole.agg, interpretationProb ~ utteranceRounded))
summary(lm(data=d.laptop.hyperbole.agg, interpretationProb ~ utteranceType))
summary(lm(data=d.laptop.hyperbole.agg, interpretationProb ~ utteranceRounded + utteranceType))
summary(lm(data=d.laptop.hyperbole.agg, interpretationProb ~ utteranceRounded * utteranceType))

##########################
# Visualize model output
##########################

m <- read.csv("../../data/model/predict_kettle_constrained.csv")
m$utterance <- factor(m$utterance)
m$meaning <- factor(m$meaning)
m$valence <- factor(m$valence)
# mark data fram with needed information for analysis
m$utteranceRounded <- factor(floor(as.numeric(as.character(m$utterance))/ 10)*10)
m$meaningRounded <- factor(floor(as.numeric(as.character(m$meaning))/ 10)*10)

m$interpretationKind <- 
  ifelse(as.numeric(m$utteranceRounded) > as.numeric(m$meaningRounded), "hyperbolic", 
         ifelse(m$utterance==m$meaning, "exact",
                ifelse(m$utteranceRounded==m$meaningRounded, "fuzzy", "other")))

# just interpretation, no opinion, full interpretation distribution

m.interp <- aggregate(data=m, probability ~ utterance + meaning, FUN=sum)

m.plot <- 
  ggplot(m.interp, aes(x=meaning, y=probability)) + 
  geom_bar(stat="identity", color="black", fill="#FF6666") + 
  facet_grid(. ~ utterance) +
  scale_x_discrete() +
  scale_y_continuous() +
  xlab("") +
  #ylab("") +
  #scale_fill_manual(values=c("#FF6666", "#FF6666"),
  #scale_fill_manual(values=c("#33CCCC", "#FF6666"),
  #                  guide=FALSE,
  #                  name="Valence",
  #                  breaks=c("1", "2"),
  #                  labels=c("No valence", "With valence")) +
                      theme_bw() +
                      ylab("Probability") +
                      xlab("Interpretation") +
                      theme(axis.title.x = element_text(size=12),
                            axis.text.x  = element_text(size=10, angle=-90),
                            axis.title.y = element_text(size=12),
                            axis.text.y = element_text(size=10))

multiplot(d.plot, m.plot)

# plot exact, fuzzy, and hyperbolic interpretations

m.interpKinds <- aggregate(data=m, probability ~ utterance + interpretationKind,
                           FUN=sum)

ggplot(m.interpKinds, aes(x=utterance, y=probability, fill=interpretationKind)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(interpretationKind~.) +
  theme_bw()

# add opinion probabilities
m.opinion <- subset(aggregate(data=m, probability ~ utterance + valence, FUN=sum), valence=="2")
colnames(m.opinion)[2] <- "interpretationKind"
m.opinion$interpretationKind <- "expensive"

m.interpKinds.opinion <- rbind(subset(m.interpKinds,interpretationKind!="other"), m.opinion)

m.interpKinds.opinion$interpretationKind <- 
  factor(m.interpKinds.opinion$interpretationKind, levels=c("exact", "fuzzy", "hyperbolic", "expensive"))

ggplot(m.interpKinds.opinion, aes(x=utterance, y=probability, fill=interpretationKind)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(interpretationKind~.) +
  theme_bw()

# human and model laptop comparison

human.laptop.interpKinds.opinion <- subset(interpretation.summary.clean, domain=="watch")
d.laptop.plot <-
  ggplot(human.laptop.interpKinds.opinion, aes(x=utterance, y=interpretationProb, fill=interpretationKind)) +
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2,
                position=position_dodge(.9)) +
                  facet_grid(interpretationKind ~.) +
                  xlab("Utterance") +
                  ylab("Probability") +
                  theme_bw() +
                  scale_fill_discrete(guid=FALSE) +
                  theme(axis.title.x = element_text(size=14),
                        axis.text.x  = element_text(size=10, angle=-90),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))

m.laptop.plot <-
  ggplot(m.interpKinds.opinion, aes(x=utterance, y=probability, fill=interpretationKind)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(interpretationKind~.) +
  theme_bw() +
  scale_fill_discrete(guid=FALSE) +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=10, angle=-90),
        axis.title.y = element_text(size=0),
        axis.text.y = element_text(size=0))

multiplot(d.laptop.plot, m.laptop.plot, cols=2)

# human model numeric comparison
human <- human.laptop.interpKinds.opinion
model <- rbind(m.interpKinds.opinion, 
               data.frame(utterance=c(50, 51), 
                          interpretationKind=c("hyperbolic", "hyperbolic"), probability=c(0,0)))
model <- model[with(model, order(utterance, interpretationKind)),]
model.comp <- 
  human[with(human, order(utterance, interpretationKind)), ]

model.comp[is.na(model.comp)] <- 0
model.comp$model <- model$probability

ggplot(model.comp, aes(x=interpretationProb, y=model, color=interpretationKind)) +
  geom_point(size=0) +
  geom_text(aes(label=utterance)) +
  theme_bw() +
  xlab("Human") +
  ylab("Model")

# ignore opinion
model.comp.no_opinion <- subset(model.comp, interpretationKind != "expensive")
ggplot(model.comp.no_opinion, aes(x=interpretationProb, y=model, color=interpretationKind)) +
  geom_point(size=0) +
  geom_text(aes(label=utterance)) +
  theme_bw() +
  xlab("Human") +
  ylab("Model")

# plot in panels
ggplot(model.comp.no_opinion, aes(x=interpretationProb, y=model, color=interpretationKind)) +
  geom_point(size=2) +
  #geom_smooth(method=lm) +
  geom_text(aes(label=utterance), size=4, position=position_jitter(width=0.01,height=0.01)) +
  facet_grid(.~interpretationKind) +
  theme_bw() +
  xlab("Human") +
  ylab("Model") +
  scale_color_discrete(guide=FALSE)

model.comp.exact <- subset(model.comp, interpretationKind=="exact")
with(model.comp.exact, cor.test(interpretationProb, model))

model.comp.fuzzy <- subset(model.comp, interpretationKind=="fuzzy")
with(model.comp.fuzzy, cor.test(interpretationProb, model))

model.comp.hyperbole <- subset(model.comp, interpretationKind=="hyperbolic")
with(model.comp.hyperbole, cor.test(interpretationProb, model))

model.comp.opinion <- subset(model.comp, interpretationKind=="expensive")
with(model.comp.opinion, cor.test(interpretationProb, model))

######
# Human model comparison for all three domains
######
# human summary data for all interpretation types and all domains
human.all <- interpretation.summary.clean
# make NaN into 0
human.all[is.na(human.all)] <- 0

# model data for all domains

m1 <- read.csv("../../data/model/predict_kettle_constrained.csv")
m1$domain <- "electric kettle"
m2 <- read.csv("../../data/model/predict_laptop_constrained.csv")
m2$domain <- "laptop"
m3 <- read.csv("../../data/model/predict_watch_constrained.csv")
m3$domain <- "watch"

m.all <- rbind(m1, m2, m3)
m.all$utterance <- factor(m.all$utterance)
m.all$meaning <- factor(m.all$meaning)
m.all$valence <- factor(m.all$valence)
# mark data fram with needed information for analysis
m.all$utteranceRounded <- factor(floor(as.numeric(as.character(m.all$utterance))/ 10)*10)
m.all$meaningRounded <- factor(floor(as.numeric(as.character(m.all$meaning))/ 10)*10)
m.all$interpretationKind <- 
  ifelse(as.numeric(m.all$utteranceRounded) > as.numeric(m.all$meaningRounded), "hyperbolic", 
         ifelse(m.all$utterance==m.all$meaning, "exact",
                ifelse(m.all$utteranceRounded==m.all$meaningRounded, "fuzzy", "other")))

######## 
# adjust probability by raising to the power of hardness and renormalizing to sum up to one within each domain/utterance pair
########
hardness = 0.6
m.all$raisedProb <- m.all$probability^hardness
normalizingFactors <- aggregate(data=m.all, raisedProb ~ domain + utterance, FUN=sum)
colnames(normalizingFactors)[3] <- "normalizing"
m.all <- join(m.all, normalizingFactors, by=c("domain", "utterance"))
m.all$adjustedProb <- m.all$raisedProb / m.all$normalizing

# aggregate model over valence and different ways to be hyperbolic
m.all.interpKinds <- aggregate(data=m.all, adjustedProb ~ domain + utterance + interpretationKind,
                           FUN=sum)
# plot model output for three domains

model.full.plot <- ggplot(m.all, aes(x=meaning, y=adjustedProb, fill=valence)) + 
  geom_bar(stat="identity", color="black") + 
  facet_grid(domain ~ utterance) +
  scale_x_discrete() +
  scale_y_continuous() +
  xlab("") +
  #ylab("") +
  #scale_fill_manual(values=c("#FF6666", "#FF6666"),
  scale_fill_manual(values=c("#33CCCC", "#FF6666"),
                    guide=FALSE,
                    name="Valence",
                    breaks=c("1", "2"),
                    labels=c("No valence", "With valence")) +
  theme_bw() +
  ylab("Probability") +
  xlab("Interpretation") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=10, angle=-90),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=10))

multiplot(human.full.plot, model.full.plot, cols=2)

# add opinion probabilities
m.all.opinion <- subset(aggregate(data=m.all, adjustedProb ~ domain + utterance + valence, FUN=sum), valence=="2")
colnames(m.all.opinion)[3] <- "interpretationKind"
m.all.opinion$interpretationKind <- "expensive"

m.all.interpKinds.opinion <- rbind(subset(m.all.interpKinds,interpretationKind!="other"), 
                                   m.all.opinion)

m.all.interpKinds.opinion$interpretationKind <- 
  factor(m.all.interpKinds.opinion$interpretationKind, levels=c("exact", "fuzzy", "hyperbolic", "expensive"))


model.all <- rbind(m.all.interpKinds.opinion, 
               data.frame(utterance=c(50,51,50,51,50,51), 
                          interpretationKind=c("hyperbolic","hyperbolic",
                                               "hyperbolic","hyperbolic","hyperbolic","hyperbolic"), 
                          adjustedProb=c(0,0,0,0,0,0), 
                          domain=c("laptop","laptop","electric kettle","electric kettle",
                                   "watch","watch")))



model.all <- model.all[with(model.all, order(domain, utterance, interpretationKind)),]
model.all.comp <- 
  human.all[with(human.all, order(domain, utterance, interpretationKind)), ]

model.all.comp$model <- model.all$adjustedProb

# ignore opinion
model.all.comp.no_opinion <- subset(model.all.comp, interpretationKind != "expensive")

# plot in panels
ggplot(model.all.comp.no_opinion, aes(x=interpretationProb, y=model, color=domain)) +
  geom_point(size=2) +
  #geom_smooth(method=lm) +
  geom_text(aes(label=utterance), size=5) +
  facet_grid(.~interpretationKind) +
  theme_bw() +
  xlab("Human") +
  ylab("Model") 
  #scale_color_discrete(guide=FALSE)

model.all.comp.exact <- subset(model.all.comp, interpretationKind=="exact")
with(model.all.comp.exact, cor.test(interpretationProb, model))

model.all.comp.fuzzy <- subset(model.all.comp, interpretationKind=="fuzzy")
with(model.all.comp.fuzzy, cor.test(interpretationProb, model))

model.all.comp.hyperbole <- subset(model.all.comp, interpretationKind=="hyperbolic")
with(model.all.comp.hyperbole, cor.test(interpretationProb, model))

#################################
# full human and model comparison (no breakdown into effects)
human.full.summary <- summarySE(d, measurevar="interpretationProb",
                          groupvars=c("utterance", "interpretation", "domain", "interpretationKind"))
human.full.summary <- human.full.summary[with(human.full.summary, order(domain, utterance, interpretation)),]
model.full.summary <- aggregate(data=m.all, adjustedProb ~ utterance + meaning + domain, FUN=sum)
model.full.summary <- model.full.summary[with(model.full.summary, order(domain, utterance, meaning)),]

compare.full.summary <- human.full.summary
colnames(compare.full.summary)[6] <- "human"
compare.full.summary$model <- model.full.summary$adjustedProb

ggplot(compare.full.summary, aes(x=human, y=model, color=interpretationKind, shape=domain)) +
  geom_point() +
  theme_bw()

with(compare.full.summary, cor.test(human, model))


###################################
# plot bar blots of effects for model and human, per domain
# set domain
dom <- "laptop"
# convert to long form
model.dom <- subset(model.all.comp.no_opinion, domain==dom)
model.dom$interpretationProb <- model.dom$model
model.dom$kind <- "model"
human.dom <- subset(model.all.comp.no_opinion, domain==dom)
human.dom$kind <- "human"
model.dom.comp <- rbind(model.dom, human.dom)

ggplot(model.dom.comp, aes(x=utterance, y=interpretationProb, fill=interpretationKind)) +
  geom_bar(stat="identity", color="black") +
  #geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2) +
  facet_grid(interpretationKind ~ kind) +
  theme_bw()

