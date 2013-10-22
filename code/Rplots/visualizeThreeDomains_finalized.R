source("summarySE.R")
source("multiplot.R")

################################################################################
# Human model comparison for all three domains
################################################################################

#######################
# human data for all domains
#######################
d <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/data1017_normalized.csv")
d$utteranceRounded <- factor(d$utteranceRounded)
d$utterance <- factor(d$utterance)
d$interpretationRounded <- factor(d$interpretationRounded)
d$interpretation <- factor(d$interpretation)

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

# human summary data for all interpretation types and all domains
human.all <- interpretation.summary.clean
# make NaN into 0
human.all[is.na(human.all)] <- 0

#######################
# model output for all domains
#######################
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

##############
# plot human and model full distributions in three domains
##############

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
model.all.comp.noOpinion <- subset(model.all.comp, interpretationKind != "expensive")

###########
# plot effects scatter plot
###########

# plot in panels
ggplot(model.all.comp.noOpinion, aes(x=interpretationProb, y=model, color=domain)) +
  geom_point(size=2) +
  #geom_smooth(method=lm) +
  geom_text(aes(label=utterance), size=6) +
  facet_grid(.~interpretationKind) +
  theme_bw() +
  xlab("Human") +
  ylab("Model") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=12))
#scale_color_discrete(guide=FALSE)

model.all.comp.exact <- subset(model.all.comp, interpretationKind=="exact")
with(model.all.comp.exact, cor.test(interpretationProb, model))

model.all.comp.fuzzy <- subset(model.all.comp, interpretationKind=="fuzzy")
with(model.all.comp.fuzzy, cor.test(interpretationProb, model))

model.all.comp.hyperbole <- subset(model.all.comp, interpretationKind=="hyperbolic")
with(model.all.comp.hyperbole, cor.test(interpretationProb, model))

#################################
# full human and model comparison (no breakdown into effects)
#################################

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
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))

with(compare.full.summary, cor.test(human, model))

###################################
# plot bar blots of effects for model and human, per domain
###################################
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

#############################
# Price priors
############################

p <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/prior_normalized.csv")
p$interpretation <- factor(p$interpretation)
p$interpretationRounded <- factor(p$interpretationRounded)
p.summary <- summarySE(data=p, measurevar="interpretationProb", groupvars=c("domain", "interpretation"))

ggplot(p.summary, aes(x=interpretation, y=interpretationProb)) +
  geom_bar(stat="identity", color="black", fill="#FF9999") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.2) +
  facet_grid(domain ~ .) +
  theme_bw() +
  xlab("Price") +
  ylab("Probability")

ggplot(p.summary, aes(x=interpretation, y=interpretationProb, fill=domain)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),
                width=0.2, position=position_dodge(0.9)) +
                  theme_bw()

p.numeric <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/prior_normalized.csv")
p.numeric.summary <- summarySE(data=p.numeric, measurevar="interpretationProb", groupvars=c("domain", "interpretationRounded"))

ggplot(p.numeric.summary, aes(x=interpretationRounded, y=interpretationProb, color=domain)) +
  geom_point(size=3) +
  #geom_text(aes(label=interpretationRounded, size=2)) +
  geom_line() +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.2) +
  theme_bw() +
  xlab("Price") +
  ylab("Probability")

### combine with hyperbole experiment data
withUtterance.summary <- summarySE(d, measurevar="interpretationProb",
                                   groupvars=c("utterance", "interpretation", "domain"))
withUtterance.summary$color <- "blue"
p.summary$utterance <- "Prior"
p.summary$color <- "red"

all.summary <- rbind(p.summary, withUtterance.summary)
all.summary$utterance <- factor(all.summary$utterance, levels=c(
  "Prior","50","51","500","501","1000","1001","5000","5001","10000","10001"))

ggplot(all.summary, aes(x=interpretation, y=interpretationProb, fill=color)) +
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.2) +
  facet_grid(domain ~ utterance) +
  theme_bw() +
  ylab("Probability") +
  xlab("Interpretation") +
  scale_fill_manual(values=c("#FF9999", "gray"), 
                    guide=FALSE) +
                      theme(axis.title.x = element_text(size=12),
                            axis.text.x  = element_text(size=10, angle=-90),
                            axis.title.y = element_text(size=12),
                            axis.text.y = element_text(size=10))

#################
# Affect priors
#################

a <- read.table("../../data/mTurkExp/hyperboleThreeDomains/affect_long.csv", 
                strip.white=TRUE, header=TRUE, sep=",")
a$utteredPriceRounded <- factor(a$utteredPriceRounded)
a.summary <- summarySE(a, measurevar="probOpinion", groupvars=c("domain", "utteredPriceRounded"))

ggplot(a.summary, aes(x=utteredPriceRounded, y=probOpinion)) +
  geom_bar(stat="identity", color="black", fill="gray") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se),width=0.2) +
  facet_grid(domain ~ .) +
  theme_bw() +
  xlab("Price") +
  ylab("Probability")

# record the affect priors

write.csv(a.summary, "../../data/mTurkExp/affectPriors/affect-10182013.csv")
