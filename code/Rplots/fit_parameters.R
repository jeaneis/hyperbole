source("summarySE.R")
source("multiplot.R")
library(ggplot2)

################################################################################
# Iterate through transformation parameters to optimize correlation
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

human.full.summary <- summarySE(d, measurevar="interpretationProb",
                                groupvars=c("utterance", "interpretation", "domain", "interpretationKind", "utteranceType", "utteranceRounded"))

###############################
# ******** Church model *********
##############################


bestFit = 0
bestHardness = 0

for (i in seq(from=0.1, to=1, by=0.01)) {
  ### Full model ###
  #c1 <- read.csv("../../data/model/church_output_kettle_lowerbound_formatted.csv")
  #c2 <- read.csv("../../data/model/church_output_laptop_lowerbound_formatted.csv")
  #c3 <- read.csv("../../data/model/church_output_watch_lowerbound_formatted.csv")
  
  c1 <- read.csv("../../data/model/church_output_kettle_formatted.csv")
  c2 <- read.csv("../../data/model/church_output_laptop_formatted.csv")
  c3 <- read.csv("../../data/model/church_output_watch_formatted.csv")
  
  c1$domain <- "electric kettle"
  c2$domain <- "laptop"
  c3$domain <- "watch"
  
  c.all <- rbind(c1, c2, c3)
  c.all$utterance <- factor(c.all$utterance)
  c.all$meaning <- factor(c.all$meaning)
  c.all$valence <- factor(c.all$valence)
  c.all$utteranceRounded <- factor(floor(as.numeric(as.character(c.all$utterance))/ 10)*10)
  c.all$meaningRounded <- factor(floor(as.numeric(as.character(c.all$meaning))/ 10)*10)
  c.all$interpretationKind <- 
    ifelse(as.numeric(c.all$utteranceRounded) > as.numeric(c.all$meaningRounded), "hyperbolic", 
           ifelse(c.all$utterance==c.all$meaning, "exact",
                  ifelse(c.all$utteranceRounded==c.all$meaningRounded, "fuzzy", "other")))
  
# Loose choice transformation by raising each probability to the power of hardness and 
# renormalizing to sum up to one within each domain/utterance pair
  c.all$raisedProb <- c.all$probability^i
  normalizingFactors <- aggregate(data=c.all, raisedProb ~ domain + utterance, FUN=sum)
  colnames(normalizingFactors)[3] <- "normalizing"
  c.all <- join(c.all, normalizingFactors, by=c("domain", "utterance"))
  c.all$adjustedProb <- c.all$raisedProb / c.all$normalizing
  
  # collapse across valence
  church.collapseOpinion <- aggregate(data=c.all, adjustedProb ~ utterance + meaning + domain + utteranceRounded +
                                        meaningRounded + interpretationKind, FUN=sum)
  #################################
  # Full human and model comparison
  #################################
  
  
  church.full.summary <- church.collapseOpinion
  #church.full.summary <- summarySE(church.collapseOpinion, 
  #                                measurevar="adjustedProb", groupvars=c("utterance", "meaning", "domain", "utteranceRounded", 
  #                                                                       "meaningRounded", "interpretationKind"))
  
  colnames(church.full.summary)[2] <- "interpretation"
  colnames(church.full.summary)[7] <- "model"
  #colnames(church.full.summary)[9] <- "model_sd"
  #colnames(church.full.summary)[10] <- "model_se"
  #colnames(church.full.summary)[11] <- "model_ci"
  
  compare.full.summary <- join(human.full.summary, church.full.summary, by=c("utterance", "interpretation", "domain", "interpretationKind"))
  colnames(compare.full.summary)[8] <- "human"
  
  r <- with(compare.full.summary, cor(human, model))
  if (r > bestFit) {
    bestFit <- r
    bestHardness <- i
  }
}

ggplot(compare.full.summary, aes(x=model, y=human)) +
  geom_point(data=compare.full.summary, aes(color=interpretationKind, shape=domain)) +
  theme_bw() +
  #geom_errorbarh(aes(xmin=model-model_se, xmax=model+model_se), color="gray") +
  geom_errorbar(aes(ymin=human-se, ymax=human+se), color="gray")

with(compare.full.summary, cor.test(model, human))

#####################
# Find best parameter to fit human data
######################


###############################
# ******** Church model *********
##############################

c1 <- read.csv("../../data/model/church_output_kettle_formatted.csv")
c2 <- read.csv("../../data/model/church_output_laptop_formatted.csv")
c3 <- read.csv("../../data/model/church_output_watch_formatted.csv")

c1$domain <- "electric kettle"
c2$domain <- "laptop"
c3$domain <- "watch"

c.all <- rbind(c1, c2, c3)
c.all$utterance <- factor(c.all$utterance)
c.all$meaning <- factor(c.all$meaning)
c.all$valence <- factor(c.all$valence)
c.all$utteranceRounded <- factor(floor(as.numeric(as.character(c.all$utterance))/ 10)*10)
c.all$meaningRounded <- factor(floor(as.numeric(as.character(c.all$meaning))/ 10)*10)
c.all$interpretationKind <- 
  ifelse(as.numeric(c.all$utteranceRounded) > as.numeric(c.all$meaningRounded), "hyperbolic", 
         ifelse(c.all$utterance==c.all$meaning, "exact",
                ifelse(c.all$utteranceRounded==c.all$meaningRounded, "fuzzy", "other")))

church.collapseOpinion <- aggregate(data=c.all, adjustedProb ~ utterance + meaning + domain + utteranceRounded +
                                      meaningRounded + interpretationKind, FUN=sum)

church.full.summary <- church.collapseOpinion


bestFit = 0
bestHardness = 0

for (i in seq(from=15, to=40, by=0.01)) {
  
  # Loose choice transformation by raising each probability to the power of hardness and 
  # renormalizing to sum up to one within each domain/utterance pair
  #######################
  # human data for all domains
  #######################
  d1 <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/data1017_stretched.csv")
  d1$utteranceRounded <- factor(d1$utteranceRounded)
  d1$utterance <- factor(d1$utterance)
  d1$interpretationRounded <- factor(d1$interpretationRounded)
  d1$interpretation <- factor(d1$interpretation)
  
  d1$interpretationKind <- 
    ifelse(as.numeric(d1$utteranceRounded) > as.numeric(d1$interpretationRounded), "hyperbolic", 
           ifelse(d1$utterance==d1$interpretation, "exact",
                  ifelse(d1$utteranceRounded==d1$interpretationRounded, "fuzzy", "other")))
  
  d1$raisedProb <- d1$interpretationProb^bestHardness
  normalizingFactors <- aggregate(data=d1, raisedProb ~ workerID + domain + utterance, FUN=sum)
  colnames(normalizingFactors)[4] <- "normalizing"
  d1 <- join(d1, normalizingFactors, by=c("workerID", "domain", "utterance"))
  d1$adjustedProb <- d1$raisedProb / d1$normalizing
  
  human.full.summary <- summarySE(d1, measurevar="adjustedProb",
                                  groupvars=c("utterance", "interpretation", "domain", "interpretationKind", "utteranceType", "utteranceRounded"))
  
  
  # collapse across valence
  #################################
  # Full human and model comparison
  #################################
  
  colnames(church.full.summary)[2] <- "interpretation"
  colnames(church.full.summary)[7] <- "model"
  #colnames(church.full.summary)[9] <- "model_sd"
  #colnames(church.full.summary)[10] <- "model_se"
  #colnames(church.full.summary)[11] <- "model_ci"
  
  compare.full.summary <- join(human.full.summary, church.full.summary, by=c("utterance", "interpretation", "domain", "interpretationKind"))
  colnames(compare.full.summary)[8] <- "human"
  
  r <- with(compare.full.summary, cor(human, model))
  if (r > bestFit) {
    bestFit <- r
    bestHardness <- i
  }
}

ggplot(compare.full.summary, aes(x=model, y=human)) +
  geom_point(data=compare.full.summary, aes(color=interpretationKind, shape=domain)) +
  theme_bw() +
  #geom_errorbarh(aes(xmin=model-model_se, xmax=model+model_se), color="gray") +
  geom_errorbar(aes(ymin=human-se, ymax=human+se), color="gray")

with(compare.full.summary, cor.test(model, human))

ggplot(compare.full.summary, aes(x=interpretation, y=human)) +
  geom_bar(stat="identity", color="black", fill="#FF9999") +
  geom_errorbar(aes(ymin=human-se, ymax=human+se),width=0.2) +
  facet_grid(domain ~ utterance) +
  theme_bw() +
  ylab("Probability") +
  xlab("Interpretation") +
  ggtitle("Human") +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=6, angle=-90),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14))