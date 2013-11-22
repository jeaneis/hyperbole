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

###############################
# ******** Church model *********
##############################

c1 <- read.csv("~/Documents/Grad_school/Research/webchurch/test/HyperboleOutput/kettle_big_formatted.csv")
c2 <- read.csv("~/Documents/Grad_school/Research/webchurch/test/HyperboleOutput/laptop_big_formatted.csv")
c3 <- read.csv("~/Documents/Grad_school/Research/webchurch/test/HyperboleOutput/watch_big_formatted.csv")


c1$domain <- "electric kettle"
c2$domain <- "laptop"
c3$domain <- "watch"
bestFit = 0
bestHardness = 0

for (i in seq(from=0.1, to=1, by=0.01)) {
  ### Full model ###
  #c1 <- read.csv("../../data/model/church_output_kettle_lowerbound_formatted.csv")
  #c2 <- read.csv("../../data/model/church_output_laptop_lowerbound_formatted.csv")
  #c3 <- read.csv("../../data/model/church_output_watch_lowerbound_formatted.csv")
  
  ### noisy model ###
  
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
  c.all$raisedProb <- c.all$probability^bestHardness
  normalizingFactors <- aggregate(data=c.all, raisedProb ~ domain + utterance + file, FUN=sum)
  colnames(normalizingFactors)[4] <- "normalizing"
  c.all <- join(c.all, normalizingFactors, by=c("domain", "utterance", "file"))
  c.all$adjustedProb <- c.all$raisedProb / c.all$normalizing
  
  # collapse across valence
  church.collapseOpinion <- aggregate(data=c.all, adjustedProb ~ file + utterance + meaning + domain + utteranceRounded +
                                        meaningRounded + interpretationKind, FUN=sum)
  #################################
  # Full human and model comparison
  #################################
  
  human.full.summary <- summarySE(d, measurevar="interpretationProb",
                                  groupvars=c("utterance", "interpretation", "domain", "interpretationKind", "utteranceType", "utteranceRounded"))
  
  #church.full.summary <- church.collapseOpinion
  church.full.summary <- summarySE(church.collapseOpinion, 
                                  measurevar="adjustedProb", groupvars=c("utterance", "meaning", "domain", "utteranceRounded", 
                                                                         "meaningRounded", "interpretationKind"))
  
  colnames(church.full.summary)[2] <- "interpretation"
  colnames(church.full.summary)[8] <- "model"
  colnames(church.full.summary)[9] <- "model_sd"
  colnames(church.full.summary)[10] <- "model_se"
  colnames(church.full.summary)[11] <- "model_ci"
  
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
  geom_errorbarh(aes(xmin=model-model_se, xmax=model+model_se), color="gray") +
  geom_errorbar(aes(ymin=human-se, ymax=human+se), color="gray")

with(compare.full.summary, cor.test(model, human))