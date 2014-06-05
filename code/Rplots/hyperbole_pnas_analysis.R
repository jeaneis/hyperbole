source("summarySE.R")
source("multiplot.R")
library(ggplot2)

################################################################################
# Human model comparison for all three domains
################################################################################

#######################
# Normalize human data by scaling first
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
######################
# Plot human full interpretation distribution
######################
full.summary1 <- summarySE(d1, measurevar="interpretationProb",
                          groupvars=c("utterance", "interpretation", "domain"))

full.summary1$interpretation <- factor(full.summary1$interpretation)
ggplot(full.summary1, aes(x=interpretation, y=interpretationProb)) +
  geom_bar(stat="identity", color="black", fill="#FF9999") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.2) +
  facet_grid(domain ~ utterance) +
  theme_bw() +
  ylab("Probability") +
  xlab("Interpretation") +
  ggtitle("Human") +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=6, angle=-90),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14))




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
######################
# Plot human full interpretation distribution
######################
full.summary <- summarySE(d, measurevar="interpretationProb",
                          groupvars=c("utterance", "interpretation", "domain"))

full.summary$interpretation <- factor(full.summary$interpretation)
ggplot(full.summary, aes(x=interpretation, y=interpretationProb, fill=domain)) +
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.2) +
  facet_grid(domain ~ utterance) +
  theme_bw() +
  ylab("Probability") +
  xlab("Interpretation") +
  #ggtitle("Human") +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=8, angle=-90),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14),
        #axis.title.x=element_text(size=16), axis.text.x=element_text(size=14),
        #axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=14), strip.text.y=element_text(size=14)) +
  #scale_color_brewer(palette="Accent")
  scale_fill_manual(values=my.colors.domains, legend=FALSE)

#######################
# STATS!!!! For human effects
#######################
interpretation.agg <- aggregate(data=d, interpretationProb ~ 
  workerID + domain + utterance + utteranceType + utteranceRounded + interpretationKind, FUN=sum)

d.exact <- subset(interpretation.agg, interpretationKind=="exact")
summary(lm(data=d.exact, interpretationProb ~ utteranceRounded))
summary(lm(data=d.exact, interpretationProb ~ utteranceType))
summary(lm(data=d.exact, interpretationProb ~ utteranceType * utteranceRounded))

d.fuzzy <- subset(interpretation.agg, interpretationKind=="fuzzy")
summary(lm(data=d.fuzzy, interpretationProb ~ utteranceRounded))
summary(lm(data=d.fuzzy, interpretationProb ~ utteranceType))
summary(lm(data=d.fuzzy, interpretationProb ~ utteranceRounded * utteranceType))

d.hyperbolic <- subset(interpretation.agg, interpretationKind=="hyperbolic")
summary(lm(data=d.hyperbolic, interpretationProb ~ utteranceRounded))
summary(lm(data=d.hyperbolic, interpretationProb ~ utteranceType))
summary(lm(data=d.hyperbolic, interpretationProb ~ utteranceType * utteranceRounded))

###############################
# ******** Church model *********
##############################

### Full model (noisy) ###
c1 <- read.csv("~/Documents/Grad_school/Research/webchurch/test/HyperboleOutput/test_kettle_formatted.csv")
c2 <- read.csv("~/Documents/Grad_school/Research/webchurch/test/HyperboleOutput/test_laptop_formatted.csv")
c3 <- read.csv("~/Documents/Grad_school/Research/webchurch/test/HyperboleOutput/test_watch_formatted.csv")

### Full model (literal is literal) ###
c1 <- read.csv("../../data/model/church_output_kettle_formatted.csv")
c2 <- read.csv("../../data/model/church_output_laptop_formatted.csv")
c3 <- read.csv("../../data/model/church_output_watch_formatted.csv")

### Lower bound ###
c1 <- read.csv("../../data/model/church_output_kettle_lowerbound_formatted.csv")
c2 <- read.csv("../../data/model/church_output_laptop_lowerbound_formatted.csv")
c3 <- read.csv("../../data/model/church_output_watch_lowerbound_formatted.csv")

### No cost ###
c1 <- read.csv("../../data/model/church_output_kettle_noCost_formatted.csv")
c2 <- read.csv("../../data/model/church_output_laptop_noCost_formatted.csv")
c3 <- read.csv("../../data/model/church_output_watch_noCost_formatted.csv")

### No prior ###
c1 <- read.csv("../../data/model/church_output_kettle_noPrior_formatted.csv")
c2 <- read.csv("../../data/model/church_output_laptop_noPrior_formatted.csv")
c3 <- read.csv("../../data/model/church_output_watch_noPrior_formatted.csv")

### No affect ###
c1 <- read.csv("../../data/model/church_output_kettle_noAffect_formatted.csv")
c2 <- read.csv("../../data/model/church_output_laptop_noAffect_formatted.csv")
c3 <- read.csv("../../data/model/church_output_watch_noAffect_formatted.csv")

### No imprecise goal ###
c1 <- read.csv("../../data/model/church_output_kettle_noImpreciseGoal_formatted.csv")
c2 <- read.csv("../../data/model/church_output_laptop_noImpreciseGoal_formatted.csv")
c3 <- read.csv("../../data/model/church_output_watch_noImpreciseGoal_formatted.csv")

### No valence goal ###
c1 <- read.csv("../../data/model/church_output_kettle_noValenceGoal_formatted.csv")
c2 <- read.csv("../../data/model/church_output_laptop_noValenceGoal_formatted.csv")
c3 <- read.csv("../../data/model/church_output_watch_noValenceGoal_formatted.csv")

###############
# Run this!!!!!!!!!!!!
###############
c1$domain <- "electric kettle"
c2$domain <- "laptop"
c3$domain <- "watch"

c.all <- rbind(c1, c2, c3)
c.all$utterance <- factor(c.all$utterance)
c.all$meaning <- factor(c.all$meaning)
c.all$valence <- factor(c.all$valence)
# mark data frame with needed information for analysis
c.all$utteranceRounded <- factor(floor(as.numeric(as.character(c.all$utterance))/ 10)*10)
c.all$meaningRounded <- factor(floor(as.numeric(as.character(c.all$meaning))/ 10)*10)
c.all$interpretationKind <- 
  ifelse(as.numeric(c.all$utteranceRounded) > as.numeric(c.all$meaningRounded), "hyperbolic", 
         ifelse(c.all$utterance==c.all$meaning, "exact",
                ifelse(c.all$utteranceRounded==c.all$meaningRounded, "fuzzy", "other")))

# Luce choice transformation by raising each probability to the power of hardness and 
# renormalizing to sum up to one within each domain/utterance pair
hardness = 0.34
c.all$raisedProb <- c.all$probability^hardness
normalizingFactors <- aggregate(data=c.all, raisedProb ~ domain + utterance, FUN=sum)
colnames(normalizingFactors)[3] <- "normalizing"
c.all <- join(c.all, normalizingFactors, by=c("domain", "utterance"))
c.all$adjustedProb <- c.all$raisedProb / c.all$normalizing

# collapse across valence
church.collapseOpinion <- aggregate(data=c.all, adjustedProb ~ utterance + meaning + domain + utteranceRounded +
  meaningRounded + interpretationKind, FUN=sum)

######################
# Plot model full interpretation distribution
######################
ggplot(church.collapseOpinion, aes(x=meaning, y=adjustedProb)) +
#ggplot(c.all, aes(x=meaning, y=adjustedProb, fill=valence)) + 
  geom_bar(stat="identity", color="black", fill="grey") + 
  facet_grid(domain ~ utterance) +
  scale_x_discrete() +
  scale_y_continuous() +
  xlab("") +
  #ylab("") +
  #scale_fill_manual(values=c("#FF6666", "#FF6666"),
  scale_fill_manual(values=c("#33CCCC", "#FF6666"),
                    guide=FALSE,
                    name="Valence",
                    breaks=c("0", "1"),
                    labels=c("No valence", "With valence")) +
                      theme_bw() +
                      ylab("Probability") +
                      xlab("Interpretation") +
                      ggtitle("Model") +
                      theme(axis.title.x = element_text(size=14),
                            axis.text.x  = element_text(size=6, angle=-90),
                            axis.title.y = element_text(size=14),
                            axis.text.y = element_text(size=10))

#################################
# Full human and model comparison
#################################

human.full.summary <- summarySE(d, measurevar="interpretationProb",
                                groupvars=c("utterance", "interpretation", "domain", "interpretationKind", "utteranceType", "utteranceRounded"))

church.full.summary <- church.collapseOpinion
colnames(church.full.summary)[2] <- "interpretation"

compare.full.summary <- join(human.full.summary, church.full.summary, by=c("utterance", "interpretation", "domain", "interpretationKind"))
colnames(compare.full.summary)[8] <- "human"
colnames(compare.full.summary)[14] <- "model"

#### CORRELATION ######
with(compare.full.summary, cor.test(human, model))
########################

#my.colors <- c(brewer.pal(3, "Dark2"), "grey")
my.colors <- c("#003399", "#339966", "#FF6633", "grey")
ggplot(compare.full.summary, aes(x=model, y=human)) +
  geom_point(size=2.5, aes(x=model, y=human, color=interpretationKind, shape=domain)) +
  #geom_abline(linetype=2) +
  #geom_text(aes(label=utterance)) +
  geom_smooth(data=compare.full.summary, aes(x=model, y=human), method=lm, color="black", linetype=2) +
  theme_bw() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=12),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.position=c(0.85, .3)) +
  xlab("Model") +
  ylab("Human") +
  scale_color_manual(values=my.colors, name="Interpretation Kind", 
                     breaks=c("exact", "fuzzy", "hyperbolic", "other"),
                     labels=c("Exact", "Fuzzy", "Hyperbolic", "Other"))+
  scale_shape_discrete(name="Item", breaks=c("electric kettle", "laptop", "watch"),
                       labels=c("Electric Kettle", "Laptop", "Watch"))



########
# Plot mmodel effects: exact, fuzzy, hyperbolic, affect
########

# aggregate model over valence and different ways to be hyperbolic
c.all.interpKinds <- aggregate(data=c.all, adjustedProb ~ domain + utterance + interpretationKind,
                               FUN=sum)
c.all.opinion <- subset(aggregate(data=c.all, adjustedProb ~ domain + utterance + valence, FUN=sum), valence=="1")
colnames(c.all.opinion)[3] <- "interpretationKind"
c.all.opinion$interpretationKind <- "expensive"

c.all.interpKinds.opinion <- rbind(subset(c.all.interpKinds,interpretationKind!="other"), 
                                   c.all.opinion)

c.all.interpKinds.opinion$interpretationKind <- 
  factor(c.all.interpKinds.opinion$interpretationKind, levels=c("exact", "fuzzy", "hyperbolic", "expensive"))


church.effects <- rbind(c.all.interpKinds.opinion, 
                    data.frame(utterance=c(50,51,50,51,50,51), 
                               interpretationKind=c("hyperbolic","hyperbolic",
                                                    "hyperbolic","hyperbolic","hyperbolic","hyperbolic"), 
                               adjustedProb=c(0,0,0,0,0,0), 
                               domain=c("laptop","laptop","electric kettle","electric kettle",
                                        "watch","watch")))


levels(church.effects$interpretationKind)<- c("Exact", "Fuzzy", "Hyperbolic", "Affective")
levels(church.effects$domain) <- c("Electric Kettle", "Laptop", "Watch")

## church model

my.colors2 <- c("#6699CC", "#66CC99", "#FF9966", "#FFCCCC")
ggplot(church.effects, aes(x=utterance, y=adjustedProb, fill=interpretationKind)) +
  geom_bar(stat="identity", color="black") +
  #geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2) +
  facet_grid(interpretationKind ~ domain) +
  theme_bw() +
  scale_fill_manual(guide=FALSE, values=my.colors2) +
  theme(strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14, angle=-90, hjust=-0.01,vjust=0.3), axis.title.x=element_text(size=16)) +
          xlab("Utterance") +
          ylab("Probability")


###############
# Halo analysis
###############

#### Human individaul ####

d.halo <- join(d.exact, d.fuzzy, by=c("workerID", "domain", "utterance"))
colnames(d.halo)[7] <- "probExact"
colnames(d.halo)[11] <- "probFuzzy"
d.halo$diff <- d.halo$probExact - d.halo$probFuzzy
d.halo.summary <- summarySE(d.halo, measurevar="diff", groupvars=c("utteranceType", "domain"))
summary(lm(data=d.halo.summary, diff ~ utteranceType))


#### Human ####
human.full.summary <- summarySE(d, measurevar="interpretationProb",
                                groupvars=c("utterance", "interpretation", "domain", "interpretationKind", "utteranceType", "utteranceRounded"))
human.full.summary <- human.full.summary[with(human.full.summary, order(domain, utterance, interpretation)),]
human.halo.exact <- subset(human.full.summary, interpretationKind=="exact")
colnames(human.halo.exact)[8] <- "probExact"
human.halo.fuzzy <- subset(human.full.summary, interpretationKind=="fuzzy")
colnames(human.halo.fuzzy)[8] <- "probFuzzy"
human.halo <- join(human.halo.exact, human.halo.fuzzy, by=c("domain", "utterance"))

human.halo$diff <- human.halo$probExact - human.halo$probFuzzy
human.halo$ratio <- ifelse((human.halo$probFuzzy + human.halo$probExact) > 0, 
                           human.halo$probExact/(human.halo$probFuzzy + human.halo$probExact), 0)
human.halo$utteranceType <- ifelse(as.numeric(as.character(human.halo$utterance)) %% 10==0, "round", "sharp")

#human.halo.summary <- summarySE(human.halo, measurevar="diff", groupvars=c("utteranceType"))
#human.halo.summary <- summarySE(human.halo, measurevar="diff", groupvars=c("utteranceType", "utteranceRounded", "domain"))
human.halo.summary <- summarySE(human.halo, measurevar="diff", groupvars=c("utteranceType", "domain"))
#human.halo.summary <- summarySE(human.halo, measurevar="ratio", groupvars=c("utteranceType", "domain"))

#########
# Stats #
#########
summary(lm(data=human.halo, diff ~ utteranceType))

## Visualize human halo ##

#ggplot(human.halo.summary, aes(x=utteranceRounded, y=diff, group=utteranceType, color=domain, shape=utteranceType, linetype=utteranceType)) +
ggplot(human.halo.summary, aes(x=utteranceType, y=diff, group=domain, color=domain, shape=utteranceType)) +
  geom_point(size=4) +
  #geom_bar(color="black", stat="identity", position=position_dodge()) +
  geom_line(linetype=2, size=1) +
  geom_errorbar(aes(ymin=diff-se, ymax=diff+se), width=0.05, color="grey")+
  theme_bw() +
  #facet_grid(domain~.) +
  ylab("P(exact) - P(fuzzy)") +
  xlab("Utterance Type") +
  theme(axis.title.x=element_text(size=16), axis.text.x=element_text(size=14), 
        axis.title.y=element_text(size=16)) +
          scale_color_brewer(palette="Accent")

# human.halo.summary <- summarySE(human.halo, measurevar="ratio", groupvars=c("utteranceType"))
# ggplot(human.halo.summary, aes(x=utteranceType, y=ratio, fill=utteranceType)) +
#   geom_bar(color="black", stat="identity", position=position_dodge()) +
#   geom_errorbar(aes(ymin=ratio-se, ymax=ratio+se), position=position_dodge(0.9), width=0.2)+
#   theme_bw() +
#   #facet_grid(domain~.) +
#   ylab("Prob(exact) / (Prob(fuzzy) + Prob(exact)")

#### Model ####

church.halo.exact <- subset(church.collapseOpinion, interpretationKind=="exact")
colnames(church.halo.exact)[7] <- "probExact"
church.halo.fuzzy <- subset(church.collapseOpinion, interpretationKind=="fuzzy")
colnames(church.halo.fuzzy)[7] <- "probFuzzy"
church.halo <- join(church.halo.exact, church.halo.fuzzy, by=c("domain", "utterance"))

church.halo$diff <- church.halo$probExact - church.halo$probFuzzy
church.halo$ratio <- ifelse((church.halo$probFuzzy + church.halo$probExact) > 0, 
                            church.halo$probExact/(church.halo$probFuzzy + church.halo$probExact), 0)
church.halo$utteranceType <- ifelse(as.numeric(as.character(church.halo$utterance)) %% 10==0, "round", "sharp")

#church.halo.summary <- summarySE(church.halo, measurevar="diff", groupvars=c("utteranceType"))
church.halo.summary <- summarySE(church.halo, measurevar="diff", groupvars=c("utteranceType", "domain"))
#church.halo.summary <- summarySE(church.halo, measurevar="diff", groupvars=c("utteranceType", "domain", "utteranceRounded"))

# Visualize model halo
#ggplot(church.halo.summary, aes(x=utteranceRounded, y=diff, group=utteranceType, color=domain, shape=utteranceType, linetype=utteranceType)) +
ggplot(church.halo.summary, aes(x=utteranceType, y=diff, group=domain, color=domain, shape=utteranceType)) +
  geom_point(size=4) +
  #geom_bar(color="black", stat="identity", position=position_dodge()) +
  geom_line(linetype=2) +
  geom_errorbar(aes(ymin=diff-se, ymax=diff+se), width=0.05, color="grey")+
  theme_bw() +
  #facet_grid(domain~.) +
  ylab("P(exact) - P(fuzzy)") +
  xlab("Utterance Type") +
  theme(axis.title.x=element_text(size=16), axis.text.x=element_text(size=14), 
        axis.title.y=element_text(size=16)) +
          scale_color_brewer(palette="Accent")

##### human + model #####

d.halo.summary$type <- "Human"
church.full.halo.summary <- church.halo.summary
church.full.halo.summary$type <-"Full model"

church.noCost.halo.summary <- church.halo.summary
church.noCost.halo.summary$type <- "Uniform utterance cost"

comp.halo.summary <- rbind(d.halo.summary, church.full.halo.summary, church.noCost.halo.summary)
comp.halo.summary$type <- factor(comp.halo.summary$type, levels=c("Human", "Full model", "Uniform utterance cost"))

# bar plot without domains
# ggplot(comp.halo.summary, aes(x=utteranceType, y=diff, fill=utteranceType)) +
#   #geom_point()+
#   geom_bar(color="black", stat="identity", position=position_dodge()) +
#   geom_errorbar(aes(ymin=diff-se, ymax=diff+se), position=position_dodge(0.9), width=0.2)+
#   theme_bw() +
#   facet_grid(.~type, scales="free_y") +
#   ylab("P(exact) - P(fuzzy)") +
#   xlab("Utterance Type") +
#   theme(axis.title.x=element_text(size=16), axis.text.x=element_text(size=14), 
#         axis.title.y=element_text(size=16), strip.text.x=element_text(size=16)) +
#           scale_fill_manual(guide=FALSE, palette="BuPu")

# scatter plot with domains
#ggplot(church.halo.summary, aes(x=utteranceRounded, y=diff, group=utteranceType, color=domain, shape=utteranceType, linetype=utteranceType)) +

my.colors.domains <- c("#ff9896", "#17becf", "#e7ba52")
ggplot(comp.halo.summary, aes(x=utteranceType, y=diff, group=domain, color=domain, shape=utteranceType)) +
  geom_point(size=5) +
  #geom_bar(color="black", stat="identity", position=position_dodge()) +
  geom_line(linetype=2, size=1) +
  geom_errorbar(aes(ymin=diff-se, ymax=diff+se), width=0.1, color="grey")+
  theme_bw() +
  facet_grid(.~type) +
  ylab("P(exact) - P(fuzzy)") +
  xlab("Utterance Type") +
  scale_shape_discrete(guide=FALSE) +
  theme(legend.title=element_text(size=0), legend.position=c(0.8, 0.85),
        axis.title.x=element_text(size=16), axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), legend.text=element_text(size=14)) +
          #scale_color_brewer(palette="Accent")
          scale_color_manual(values=my.colors.domains)

############
# Hyperbole analysis
############

### Human ###

human.hyperbole <- subset(d, interpretationKind=="hyperbolic")
human.hyperbole.agg <- aggregate(data=human.hyperbole, interpretationProb ~ utterance + utteranceType + utteranceRounded + domain + workerID, FUN=sum)

human.hyperbole.summary <- summarySE(human.hyperbole.agg, measurevar="interpretationProb", 
                                     groupvars=c("utteranceRounded", "domain"))

#########
# Stats #
#########

p <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/prior_normalized.csv")
p$interpretation <- factor(p$interpretation)
p$interpretationRounded <- factor(p$interpretationRounded)
price.prior <- summarySE(data=p, measurevar="interpretationProb", groupvars=c("domain", "interpretationRounded"))[,1:4]
colnames(price.prior)[2] <- "utteranceRounded"
colnames(price.prior)[4] <- "prior"

human.hyperbole.prior <- join(human.hyperbole.summary, price.prior, by=c("domain", "utteranceRounded"))

summary(lm(data=human.hyperbole.prior, interpretationProb ~ prior))

# visualize human hyperbole
ggplot(human.hyperbole.summary, aes(x=utteranceRounded, y=interpretationProb, group=domain, color=domain)) +
  geom_point() +
  geom_line() +
  #geom_text(aes(label=utteranceRounded, color=domain)) +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se), width=0.05, color="grey") +
  theme_bw() +
  xlab("Utterance rounded") +
  ylab("P(hyperbole)") +
  theme(legend.title=element_text(size=0), legend.position=c(0.85, 0.85),
        axis.title.x=element_text(size=16), axis.text.x=element_text(size=14, angle=-90),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), legend.text=element_text(size=14)) +
          scale_color_brewer(palette="Accent")

### Model ###

church.hyperbole <- subset(church.collapseOpinion, interpretationKind=="hyperbolic")
church.hyperbole.agg <- aggregate(data=church.hyperbole, adjustedProb ~ utterance + domain + utteranceRounded, FUN=sum)
church.hyperbole.agg$domain <- factor(church.hyperbole.agg$domain)

church.hyperbole.summary <- summarySE(church.hyperbole.agg, measurevar="adjustedProb",
                                      groupvars=c("utteranceRounded", "domain"))

colnames(church.hyperbole.summary)[4] <- "interpretationProb"

## visualize model
ggplot(church.hyperbole.summary, aes(x=utteranceRounded, y=interpretationProb, group=domain, color=domain)) +
  geom_point() +
  geom_line() +
  #geom_text(aes(label=utteranceRounded, color=domain)) +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se), width=0.05, color="grey") +
  theme_bw() +
  xlab("Utterance rounded") +
  ylab("P(hyperbole)") +
  theme(legend.title=element_text(size=0), legend.position=c(0.85, 0.85),
        axis.title.x=element_text(size=16), axis.text.x=element_text(size=14, angle=-90),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), legend.text=element_text(size=14)) +
          scale_color_brewer(palette="Accent")

### human + model ###

human.hyperbole.summary$type <- "Human"
human.hyperbole.summary$utteranceRounded <- factor(human.hyperbole.summary$utteranceRounded)
church.full.hyperbole.summary <- church.hyperbole.summary
church.full.hyperbole.summary$type <- "Full model"

church.noPrior.hyperbole.summary <- church.hyperbole.summary
church.noPrior.hyperbole.summary$type <- "Uniform price prior"

comp.hyperbole.summary <- rbind(human.hyperbole.summary, church.full.hyperbole.summary, church.noPrior.hyperbole.summary)
comp.hyperbole.summary$type <- factor(comp.hyperbole.summary$type, levels=c("Human", "Full model", "Uniform price prior"))

comp.hyperbole.summary$utteranceRounded <- factor(comp.hyperbole.summary$utteranceRounded)

ggplot(comp.hyperbole.summary, aes(x=utteranceRounded, y=interpretationProb, group=domain, color=domain)) +
  geom_point(size=5) +
  geom_line(aes(color=domain), size=1) +
  #geom_text(aes(label=utterance, color=domain)) +
  facet_grid(.~type) +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se), width=0.2, color="grey") +
  theme_bw() +
  xlab("Utterance rounded") +
  ylab("P(hyperbole)") +
  theme(legend.title=element_text(size=0), legend.position=c(0.8, 0.85),
        axis.title.x=element_text(size=16), axis.text.x=element_text(size=14, angle=-90),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), legend.text=element_text(size=14)) +
          scale_color_manual(values=my.colors.domains)


#######
# Affect analysis
#######

### human ###
ap <- read.table("../../data/mTurkExp/hyperboleThreeDomains/affect_pairs_160_long.csv", 
                 strip.white=TRUE, header=TRUE, sep=",")
ap$actualPriceSharpened <- ifelse(ap$actualType=="sharp", ap$actualPriceRounded+1, ap$actualPriceRounded)
ap$utteredPriceSharpened <- ifelse(ap$utteredType=="sharp", ap$utteredPriceRounded+1, ap$utteredPriceRounded)
ap$actualPriceRounded <- factor(ap$actualPriceRounded)
ap$utteredPriceRounded <- factor(ap$utteredPriceRounded)
ap$actualPriceSharpened <- factor(ap$actualPriceSharpened)
ap$utteredPriceSharpened <- factor(ap$utteredPriceSharpened)
ap$isHyperbole <- ifelse(ap$actualPriceRounded!=ap$utteredPriceRounded,
                         "hyperbole", "literal")

#################
# Aggregate across different hyperbolic utterances
#################
######
# Stats for human affect effect
######

ap.summary <- summarySE(ap, measurevar="probOpinion", 
                        groupvars=c("actualPriceRounded", "isHyperbole", "domain"))

summary(lm(data=ap.summary, probOpinion ~ isHyperbole))

ggplot(ap.summary, aes(x=actualPriceRounded, y=probOpinion, group=isHyperbole, color=domain, shape=isHyperbole, linetype=isHyperbole)) +
  geom_point(size=3) +
  geom_line() +
  #geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), width=0.2) +
  facet_grid(domain ~ .) +
  theme_bw() +
  theme(axis.text.x  = element_text(size=10, angle=-90)) +
  scale_shape_manual(values=c(15, 6)) +
  scale_color_manual(values=my.colors.domains, guide=FALSE)

### model ###

c.all.withAffect <- subset(c.all, valence=="1")
colnames(c.all.withAffect)[11] <- "affectProb"
c.all.noAffect <- subset(c.all, valence=="0")
colnames(c.all.noAffect)[11] <- "noAffectProb"
c.affect <- join(c.all.withAffect, c.all.noAffect, by=c("utterance", "meaning", "utteranceRounded", "meaningRounded", "domain", "interpretationKind"))
c.affect$affectRatio <- c.affect$affectProb / (c.affect$affectProb + c.affect$noAffectProb)

church.affect <- subset(c.affect, as.numeric(as.character(c.affect$utteranceRounded)) >=
  as.numeric(as.character(c.affect$meaningRounded)))

church.affect$isHyperbole <- ifelse(church.affect$utteranceRounded==church.affect$meaningRounded, "literal", "hyperbole")

church.affect.summary <- summarySE(church.affect, measurevar="affectRatio", groupvars=c("domain", "meaningRounded", "isHyperbole"))

colnames(church.affect.summary)[2] <- "actualPriceRounded"
colnames(church.affect.summary)[5] <- "probOpinion"

human.affect.summary <- ap.summary
human.affect.summary$type <- "Human"
church.full.affect.summary <- church.affect.summary
church.full.affect.summary$type <- "Full model"
church.noAffectPrior.affect.summary <- church.affect.summary
church.noAffectPrior.affect.summary$type <- "Uniform affect prior"

comp.affect.summary <- rbind(human.affect.summary, church.full.affect.summary, church.noAffectPrior.affect.summary)
comp.affect.summary$type <- factor(comp.affect.summary$type, levels=c("Human", "Full model", "Uniform affect prior"))
colnames(comp.affect.summary)[2] <- "Literalness"

ggplot(comp.affect.summary, aes(x=actualPriceRounded, y=probOpinion, group=Literalness, color=domain, shape=Literalness, linetype=Literalness)) +
  geom_point(size=5) +
  geom_line(size=1) +
  #geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), width=0.2, color="dark gray") +
  facet_grid(domain ~ type) +
  theme_bw() +
  xlab("Price state rounded") +
  ylab("P(affect | utterance and price state)") +
  scale_shape_manual(values=c(8, 16)) +
  scale_linetype_manual(values=c(2, 1)) +
  scale_color_manual(values=my.colors.domains, guide=FALSE) +
  theme(axis.title.x=element_text(size=16), axis.text.x=element_text(size=14, angle=-90),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        legend.title=element_text(size=0), legend.text=element_text(size=14),
        legend.position=c(0.9, 0.9))

#################
# Scatter plot for each utterance rounded / meaning rounded pair
#################

human.pair <- summarySE(ap, measurevar="probOpinion", groupvars=c("utteredPriceRounded", "actualPriceRounded", "isHyperbole", "domain"))
model.pair <- summarySE(church.affect, measurevar="affectRatio", groupvars=c("utteranceRounded", "meaningRounded", "isHyperbole", "domain"))
colnames(human.pair)[1] <- "utteranceRounded"
colnames(human.pair)[2] <- "meaningRounded"
colnames(human.pair)[6] <- "humanAffect"
colnames(model.pair)[6] <- "modelAffect"

# human
ggplot(human.pair, aes(x=meaningRounded, y=humanAffect, fill=isHyperbole)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(domain~utteranceRounded) +
  geom_errorbar(aes(ymin=humanAffect-se, ymax=humanAffect+se), width=0.2) +
  theme_bw()

# model
ggplot(model.pair, aes(x=meaningRounded, y=modelAffect, fill=isHyperbole)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(domain~utteranceRounded) +
  theme_bw()


model.pair$se <- NULL
comp.pair <- join(human.pair, model.pair, by=c("utteranceRounded", "meaningRounded", "isHyperbole", "domain"))
comp.pair$label <- paste(comp.pair$utteranceRounded, comp.pair$meaningRounded, sep=",")

ggplot(comp.pair, aes(x=modelAffect, y=humanAffect)) +
  #geom_text(aes(label=label), color="dark grey") +
  geom_point(data=comp.pair, aes(x=modelAffect, y=humanAffect, color=isHyperbole, shape=domain), size=3) +
  geom_smooth(data=comp.pair, aes(x=modelAffect, y=humanAffect), method=lm, color="black", linetype=2) +
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
# 0.7717
  
with(comp.pair, cor.test(modelAffect, humanAffect))


### no collapsing

human.pair.noCollapse <- summarySE(ap, measurevar="probOpinion", groupvars=c("utteredPriceSharpened", "actualPriceSharpened", "isHyperbole", "domain"))
model.pair.noCollapse <- summarySE(church.affect, measurevar="affectRatio", groupvars=c("utterance", "meaning", "isHyperbole", "domain"))
colnames(human.pair.noCollapse)[1] <- "utterance"
colnames(human.pair.noCollapse)[2] <- "meaning"
colnames(human.pair.noCollapse)[6] <- "humanAffect"
colnames(model.pair.noCollapse)[6] <- "modelAffect"

# human
ggplot(human.pair.noCollapse, aes(x=meaning, y=humanAffect, fill=isHyperbole)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(domain~utterance) +
  geom_errorbar(aes(ymin=humanAffect-se, ymax=humanAffect+se), width=0.2) +
  theme_bw()

# model
ggplot(model.pair.noCollapse, aes(x=meaning, y=modelAffect, fill=isHyperbole)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(domain~utterance) +
  theme_bw()


model.pair.noCollapse$se <- NULL
comp.pair.noCollapse <- join(human.pair.noCollapse, model.pair.noCollapse, 
                             by=c("utterance", "meaning", "isHyperbole", "domain"))
comp.pair.noCollapse$label <- 
  paste(comp.pair.noCollapse$utterance, comp.pair.noCollapse$meaning, sep=",")

ggplot(comp.pair.noCollapse, aes(x=modelAffect, y=humanAffect)) +
  #geom_text(aes(label=label), color="dark grey") +
  geom_point(data=comp.pair.noCollapse, aes(x=modelAffect, y=humanAffect, color=isHyperbole, shape=domain), size=3) +
  geom_smooth(data=comp.pair.noCollapse, aes(x=modelAffect, y=humanAffect), method=lm, color="black", linetype=2) +
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
# 0.7717

with(comp.pair.noCollapse, cor.test(modelAffect, humanAffect))

############
# Split-half correlation for humans
############

sumR = 0
for (i in 1:100) {
  selected<-sample(nrow(ap),nrow(ap)/2)
  ap.sample1 <- ap[selected,] 
  ap.sample2 <- ap[-selected,]
  ap.sample1.summary <- summarySE(ap.sample1, measurevar="probOpinion", 
                                groupvars=c("utteredPriceRounded", "actualPriceRounded", "isHyperbole", "domain"))
  ap.sample2.summary <- summarySE(ap.sample2, measurevar="probOpinion",
                                groupvars=c("utteredPriceRounded", "actualPriceRounded", "isHyperbole", "domain"))

  sumR = sumR + cor(ap.sample1.summary$probOpinion, ap.sample2.summary$probOpinion)
}

sumR / 100

#########
# Goals analysis
#########

### human ###
this.domain <- "electric kettle"
this.utterance <- "1000"
human.example <- subset(human.full.summary, domain==this.domain & utterance==this.utterance)
human.example$N <- NULL
human.example$utteranceType <- NULL
human.example$type <- "Human"

### model ###
church.example <- subset(church.collapseOpinion, domain==this.domain & utterance==this.utterance)
colnames(church.example)[2] <- "interpretation"
colnames(church.example)[7] <- "interpretationProb"
church.example$meaningRounded <- NULL
church.example$sd <- 0
church.example$se <- 0
church.example$ci <- 0

### human + model ###
church.full.example <- church.example
church.full.example$type <- "Full model"

church.literal.example <- data.frame(utterance=c("1000"), interpretation=c("1000"), domain=c("watch"),
                                     utteranceRounded=c("10000"), interpretationKind=c("exact"), 
                                     interpretationProb=c(1), sd=c(0), se=c(0), ci=c(0), type=c("No goals"))

church.noValence.example <- church.example
church.noValence.example$type <- "Imprecise goal"

church.noImprecise.example <- church.example
church.noImprecise.example$type <- "Affect goal"

comp.goal.example <- rbind(human.example, church.full.example, church.literal.example, 
                           church.noImprecise.example, church.noValence.example)

comp.goal.example$type <- factor(comp.goal.example$type, levels=
  c("No goals", "Imprecise goal", "Affect goal", "Full model", "Human"))

#my.colors.goals <- c("#d9d9d9", "#dadaeb", "#bcbddc", "#9edae5", "#17becf")
my.colors.goals <- c("#d9d9d9", "#dadaeb", "#bcbddc", "#9edae5", "#ff9896")
ggplot(comp.goal.example, aes(x=interpretation, y=interpretationProb, fill=type)) +
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se), width=0.2) +
  theme_bw() +
  facet_grid(.~type) +
  scale_fill_manual(values=my.colors.goals, guide=FALSE) +
  #geom_text("text", x=1, y=2, label='"The electric kettle cost 1,000 dollars."') +
  ggtitle('"The electric kettle cost 1,000 dollars."') +
  xlab("Interpretation") +
  ylab("Probability") +
  theme(axis.title.x=element_text(size=16), axis.text.x=element_text(size=14, angle=-90, vjust=0.3),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        legend.text=element_text(size=0),title=element_text(size=18))


#############################
# Price priors
############################

p <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/prior_normalized.csv")
p$interpretation <- factor(p$interpretation)
p$interpretationRounded <- factor(p$interpretationRounded)
p.summary <- summarySE(data=p, measurevar="interpretationProb", groupvars=c("domain", "interpretation"))

ggplot(p.summary, aes(x=interpretation, y=interpretationProb)) +
  #geom_bar(stat="identity", color="black", fill="#FF9999") +
  geom_bar(stat="identity", color="black", fill="gray") +
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
