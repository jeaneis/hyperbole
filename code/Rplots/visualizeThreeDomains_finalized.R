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

#######################
# STATS!!!! Check this!
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

######################

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
  ggtitle("Human") +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=6, angle=-90),
        axis.title.y = element_text(size=0),
        axis.text.y = element_text(size=0))

# human summary data for all interpretation types and all domains
human.all <- interpretation.summary.clean
# make NaN into 0
human.all[is.na(human.all)] <- 0

###############
# Human halo analysis
###############
# halo analysis on subject level
d.halo.exact <- subset(d, interpretationKind=="exact")
colnames(d.halo.exact)[18] <- "probExact"
d.halo.fuzzy <- subset(d, interpretationKind=="fuzzy")
colnames(d.halo.fuzzy)[18] <- "probFuzzy"
d.halo <- join(d.halo.exact, d.halo.fuzzy, by=c("workerID", "utteranceID", "domain", "utteranceType",
                                                "utteranceRounded", "utterance",
                                                "interpretationRounded"))
d.halo$diff <- d.halo$probExact - d.halo$probFuzzy
d.halo$ratio <- ifelse((d.halo$probFuzzy + d.halo$probExact) > 0, 
                       d.halo$probExact/(d.halo$probFuzzy + d.halo$probExact), 0)

d.halo.summary <- summarySE(d.halo, measurevar="diff", groupvars=c("utteranceType"))
ggplot(d.halo.summary, aes(x=utteranceType, y=diff, fill=utteranceType)) +
  geom_bar(color="black", stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=diff-se, ymax=diff+se), position=position_dodge(0.9), width=0.2)+
  theme_bw() +
  #facet_grid(domain~.) +
  ylab("Prob(exact) - Prob(fuzzy)")

d.halo.summary <- summarySE(d.halo, measurevar="ratio", groupvars=c("utteranceType"))
ggplot(d.halo.summary, aes(x=utteranceType, y=ratio, fill=utteranceType)) +
  geom_bar(color="black", stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ratio-se, ymax=ratio+se), position=position_dodge(0.9), width=0.2)+
  theme_bw() +
  #facet_grid(domain~.) +
  ylab("Prob(exact) / (Prob(fuzzy) + Prob(exact)")

#########
# Halo anlaysis aggregate subjects
#########
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
human.halo.summary <- summarySE(human.halo, measurevar="ratio", groupvars=c("utteranceType", "domain"))
#human.halo.summary <- summarySE(human.halo, measurevar="diff", groupvars=c("utteranceType", "utteranceRounded"))
ggplot(human.halo.summary, aes(x=utteranceType, y=ratio, group=domain, color=domain)) +
  geom_point(size=3) +
  #geom_bar(color="black", stat="identity", position=position_dodge()) +
  geom_line() +
  geom_errorbar(aes(ymin=ratio-se, ymax=ratio+se), width=0.05)+
  theme_bw() +
  #facet_grid(domain~.) +
  ylab("P(exact) - P(fuzzy)") +
  xlab("Utterance Type") +
  theme(axis.title.x=element_text(size=16), axis.text.x=element_text(size=14), 
        axis.title.y=element_text(size=16)) +
          scale_color_brewer(palette="Accent")
          #scale_fill_manual(values=c("grey", "grey"), guide=FALSE) +

# human.halo.summary <- summarySE(human.halo, measurevar="ratio", groupvars=c("utteranceType"))
# ggplot(human.halo.summary, aes(x=utteranceType, y=ratio, fill=utteranceType)) +
#   geom_bar(color="black", stat="identity", position=position_dodge()) +
#   geom_errorbar(aes(ymin=ratio-se, ymax=ratio+se), position=position_dodge(0.9), width=0.2)+
#   theme_bw() +
#   #facet_grid(domain~.) +
#   ylab("Prob(exact) / (Prob(fuzzy) + Prob(exact)")

############
# Hyperbole analysis humans
############

human.hyperbole <- subset(d, interpretationKind=="hyperbolic")
human.hyperbole.agg <- aggregate(data=human.hyperbole, interpretationProb ~ utterance + utteranceType + utteranceRounded + domain + workerID, FUN=sum)

#human.hyperbole.summary <- summarySE(human.hyperbole.agg, measurevar="interpretationProb", 
#                                     groupvars=c("utterance", "domain"))

human.hyperbole.summary <- summarySE(human.hyperbole.agg, measurevar="interpretationProb", 
                                     groupvars=c("utteranceRounded", "domain"))

p <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/prior_normalized.csv")
p$interpretation <- factor(p$interpretation)
p$interpretationRounded <- factor(p$interpretationRounded)
price.prior <- summarySE(data=p, measurevar="interpretationProb", groupvars=c("domain", "interpretationRounded"))[,1:4]
colnames(price.prior)[2] <- "utteranceRounded"
colnames(price.prior)[4] <- "prior"

human.hyperbole.summary <- join(human.hyperbole.summary, price.prior, by=c("utteranceRounded", "domain"))
#human.hyperbole.summary$utterance <- as.numeric(as.character(human.hyperbole.summary$utterance))

ggplot(human.hyperbole.summary, aes(x=utteranceRounded, y=interpretationProb)) +
  geom_point() +
  #geom_text(aes(label=utteranceRounded, color=domain)) +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se), width=0.005, color="grey") +
  theme_bw()


# #######################
# # model output for all domains
# #######################
# m1 <- read.csv("../../data/model/predict_kettle_constrained.csv")
# m1$domain <- "electric kettle"
# m2 <- read.csv("../../data/model/predict_laptop_constrained.csv")
# m2$domain <- "laptop"
# m3 <- read.csv("../../data/model/predict_watch_constrained.csv")
# m3$domain <- "watch"
# 
# m.all <- rbind(m1, m2, m3)
# m.all$utterance <- factor(m.all$utterance)
# m.all$meaning <- factor(m.all$meaning)
# m.all$valence <- factor(m.all$valence)
# # mark data fram with needed information for analysis
# m.all$utteranceRounded <- factor(floor(as.numeric(as.character(m.all$utterance))/ 10)*10)
# m.all$meaningRounded <- factor(floor(as.numeric(as.character(m.all$meaning))/ 10)*10)
# m.all$interpretationKind <- 
#   ifelse(as.numeric(m.all$utteranceRounded) > as.numeric(m.all$meaningRounded), "hyperbolic", 
#          ifelse(m.all$utterance==m.all$meaning, "exact",
#                 ifelse(m.all$utteranceRounded==m.all$meaningRounded, "fuzzy", "other")))
# 
# ######## 
# # adjust probability by raising to the power of hardness and renormalizing to sum up to one within each domain/utterance pair
# ########
# hardness = 0.6
# m.all$raisedProb <- m.all$probability^hardness
# normalizingFactors <- aggregate(data=m.all, raisedProb ~ domain + utterance, FUN=sum)
# colnames(normalizingFactors)[3] <- "normalizing"
# m.all <- join(m.all, normalizingFactors, by=c("domain", "utterance"))
# m.all$adjustedProb <- m.all$raisedProb / m.all$normalizing
# 
# # aggregate model over valence and different ways to be hyperbolic
# m.all.interpKinds <- aggregate(data=m.all, adjustedProb ~ domain + utterance + interpretationKind,
#                                FUN=sum)
# # plot model output for three domains
# model.full.plot <- ggplot(m.all, aes(x=meaning, y=adjustedProb, fill=valence)) + 
#   geom_bar(stat="identity", color="black") + 
#   facet_grid(domain ~ utterance) +
#   scale_x_discrete() +
#   scale_y_continuous() +
#   xlab("") +
#   #ylab("") +
#   #scale_fill_manual(values=c("#FF6666", "#FF6666"),
#   scale_fill_manual(values=c("#33CCCC", "#FF6666"),
#                     guide=FALSE,
#                     name="Valence",
#                     breaks=c("1", "2"),
#                     labels=c("No valence", "With valence")) +
#                       theme_bw() +
#                       ylab("Probability") +
#                       xlab("Interpretation") +
#                       theme(axis.title.x = element_text(size=12),
#                             axis.text.x  = element_text(size=6, angle=-90),
#                             axis.title.y = element_text(size=12),
#                             axis.text.y = element_text(size=10))

#################
# Church model
#################
c1 <- read.csv("../../data/model/church_output_kettle_formatted.csv")
c1$domain <- "electric kettle"
c2 <- read.csv("../../data/model/church_output_laptop_formatted.csv")
c2$domain <- "laptop"
c3 <- read.csv("../../data/model/church_output_watch_formatted.csv")
c3$domain <- "watch"

c.all <- rbind(c1, c2, c3)
c.all$utterance <- factor(c.all$utterance)
c.all$meaning <- factor(c.all$meaning)
c.all$valence <- factor(c.all$valence)
# mark data fram with needed information for analysis
c.all$utteranceRounded <- factor(floor(as.numeric(as.character(c.all$utterance))/ 10)*10)
c.all$meaningRounded <- factor(floor(as.numeric(as.character(c.all$meaning))/ 10)*10)
c.all$interpretationKind <- 
  ifelse(as.numeric(c.all$utteranceRounded) > as.numeric(c.all$meaningRounded), "hyperbolic", 
         ifelse(c.all$utterance==c.all$meaning, "exact",
                ifelse(c.all$utteranceRounded==c.all$meaningRounded, "fuzzy", "other")))


######## 
# adjust probability by raising to the power of hardness and renormalizing to sum up to one within each domain/utterance pair
########
hardness = 0.5
c.all$raisedProb <- c.all$probability^hardness
normalizingFactors <- aggregate(data=c.all, raisedProb ~ domain + utterance, FUN=sum)
colnames(normalizingFactors)[3] <- "normalizing"
c.all <- join(c.all, normalizingFactors, by=c("domain", "utterance"))
c.all$adjustedProb <- c.all$raisedProb / c.all$normalizing

# aggregate model over valence and different ways to be hyperbolic
c.all.interpKinds <- aggregate(data=c.all, adjustedProb ~ domain + utterance + interpretationKind,
                               FUN=sum)
# plot model output for three domains
church.full.plot <- ggplot(c.all, aes(x=meaning, y=adjustedProb, fill=valence)) + 
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

########
# Model halo analysis
########

church.collapseOpinion <- aggregate(data=c.all, adjustedProb ~ utterance + meaning + domain + utteranceRounded +
  meaningRounded + interpretationKind, FUN=sum)
church.halo.exact <- subset(church.collapseOpinion, interpretationKind=="exact")
colnames(church.halo.exact)[7] <- "probExact"
church.halo.fuzzy <- subset(church.collapseOpinion, interpretationKind=="fuzzy")
colnames(church.halo.fuzzy)[7] <- "probFuzzy"
church.halo <- join(church.halo.exact, church.halo.fuzzy, by=c("domain", "utterance"))

church.halo$diff <- church.halo$probExact - church.halo$probFuzzy
church.halo$ratio <- ifelse((church.halo$probFuzzy + church.halo$probExact) > 0, 
                           church.halo$probExact/(church.halo$probFuzzy + church.halo$probExact), 0)
church.halo$utteranceType <- ifelse(as.numeric(as.character(church.halo$utterance)) %% 10==0, "round", "sharp")

church.halo.summary <- summarySE(church.halo, measurevar="diff", groupvars=c("utteranceType"))
ggplot(church.halo.summary, aes(x=utteranceType, y=diff, fill=utteranceType)) +
  geom_bar(color="black", stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=diff-se, ymax=diff+se), position=position_dodge(0.9), width=0.2)+
  theme_bw() +
  #facet_grid(domain~.) +
  ylab("Prob(exact) - Prob(fuzzy)")

######
# plot halo human + model + bad model
######

human.halo.summary$type <- "Human"
church.full.halo.summary <- church.halo.summary
church.full.halo.summary$type <-"Full model"
church.noCost.halo.summary <- church.halo.summary
church.noCost.halo.summary$type <- "Uniform utterance cost"

comp.halo.summary <- rbind(human.halo.summary, church.full.halo.summary, church.noCost.halo.summary)
comp.halo.summary$type <- factor(comp.halo.summary$type, levels=c("Human", "Full model", "Uniform utterance cost"))

ggplot(comp.halo.summary, aes(x=utteranceType, y=diff, fill=utteranceType)) +
  #geom_point()+
  geom_bar(color="black", stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=diff-se, ymax=diff+se), position=position_dodge(0.9), width=0.2)+
  theme_bw() +
  facet_grid(.~type, scales="free_y") +
  ylab("P(exact) - P(fuzzy)") +
  xlab("Utterance Type") +
  theme(axis.title.x=element_text(size=16), axis.text.x=element_text(size=14), 
        axis.title.y=element_text(size=16), strip.text.x=element_text(size=16)) +
          scale_fill_manual(guide=FALSE, palette="BuPu")

########
# Plot hyperbole human, model, and bad model comparison
########

p <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/prior_normalized.csv")
p$interpretation <- factor(p$interpretation)
p$interpretationRounded <- factor(p$interpretationRounded)
price.prior <- summarySE(data=p, measurevar="interpretationProb", groupvars=c("domain", "interpretationRounded"))[,1:4]
colnames(price.prior)[2] <- "utteranceRounded"
colnames(price.prior)[4] <- "prior"

church.hyperbole <- subset(church.collapseOpinion, interpretationKind=="hyperbolic")
church.hyperbole.agg <- aggregate(data=church.hyperbole, adjustedProb ~ utterance + domain + utteranceRounded, FUN=sum)
church.hyperbole.agg$domain <- factor(church.hyperbole.agg$domain)

church.hyperbole.summary <- summarySE(church.hyperbole.agg, measurevar="adjustedProb",
                                      groupvars=c("utteranceRounded", "domain"))
  
church.hyperbole.summary <-  join(church.hyperbole.summary, price.prior, by=c("utteranceRounded", "domain"))
#church.hyperbole.summary$utterance <- as.numeric(as.character(church.hyperbole.summary$utterance))

human.hyperbole.summary$type <- "Human"
human.hyperbole.summary$utteranceRounded <- factor(human.hyperbole.summary$utteranceRounded)
#human.hyperbole.summary$N <- NULL
#human.hyperbole.summary$utterance <- NULL
church.full.hyperbole.summary <- church.hyperbole.summary
church.full.hyperbole.summary$type <- "Full model"
#church.full.hyperbole.summary$sd <- 0
#church.full.hyperbole.summary$se <- 0
#church.full.hyperbole.summary$ci <- 0
colnames(church.full.hyperbole.summary)[4] <- "interpretationProb"
church.noPrior.hyperbole.summary <- church.hyperbole.summary
church.noPrior.hyperbole.summary$type <- "Uniform price prior"
#church.noPrior.hyperbole.summary$sd <- 0
#church.noPrior.hyperbole.summary$se <- 0
#church.noPrior.hyperbole.summary$ci <- 0
colnames(church.noPrior.hyperbole.summary)[4] <- "interpretationProb"

comp.hyperbole.summary <- rbind(human.hyperbole.summary, church.full.hyperbole.summary, church.noPrior.hyperbole.summary)
comp.hyperbole.summary$type <- factor(comp.hyperbole.summary$type, levels=c("Human", "Full model", "Uniform price prior"))

comp.hyperbole.summary$utteranceRounded <- factor(comp.hyperbole.summary$utteranceRounded)

ggplot(comp.hyperbole.summary, aes(x=utteranceRounded, y=interpretationProb, group=domain)) +
  geom_point() +
  geom_line(aes(color=domain)) +
  #geom_text(aes(label=utterance, color=domain)) +
  facet_grid(.~type) +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se), width=0.2, color="grey") +
  theme_bw() +
  xlab("Utterance rounded") +
  ylab("P(hyperbole)") +
  theme(legend.title=element_text(size=0), legend.position=c(0.85, 0.85),
        axis.title.x=element_text(size=16), axis.text.x=element_text(size=14, angle=-90),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), legend.text=element_text(size=14)) +
          scale_color_brewer(palette="Accent")

##############
# plot human and model full distributions in three domains
##############

#multiplot(church.full.plot, human.full.plot, cols=2)

# add opinion probabilities
c.all.opinion <- subset(aggregate(data=c.all, adjustedProb ~ domain + utterance + valence, FUN=sum), valence=="1")
colnames(c.all.opinion)[3] <- "interpretationKind"
c.all.opinion$interpretationKind <- "expensive"

c.all.interpKinds.opinion <- rbind(subset(c.all.interpKinds,interpretationKind!="other"), 
                                   c.all.opinion)

c.all.interpKinds.opinion$interpretationKind <- 
  factor(c.all.interpKinds.opinion$interpretationKind, levels=c("exact", "fuzzy", "hyperbolic", "expensive"))


church.all <- rbind(c.all.interpKinds.opinion, 
                   data.frame(utterance=c(50,51,50,51,50,51), 
                              interpretationKind=c("hyperbolic","hyperbolic",
                                                   "hyperbolic","hyperbolic","hyperbolic","hyperbolic"), 
                              adjustedProb=c(0,0,0,0,0,0), 
                              domain=c("laptop","laptop","electric kettle","electric kettle",
                                       "watch","watch")))



church.all <- church.all[with(church.all, order(domain, utterance, interpretationKind)),]
church.all.comp <- 
  human.all[with(human.all, order(domain, utterance, interpretationKind)), ]

#church.all.comp$model <- church.all$adjustedProb
church.all.comp <- join(church.all.comp, church.all, by=c("domain", "utterance", "interpretationKind"))
colnames(church.all.comp)[9] <- "model"
church.all.comp[is.na(church.all.comp)] <- 0

#church.all.comp$oldModel <- model.all$adjustedProb
church.all.comp.noOpinion <- subset(church.all.comp, interpretationKind != "expensive")

###########
# plot effects scatter plot
###########

# plot in panels
ggplot(church.all.comp.noOpinion, aes(x=interpretationProb, y=model, color=domain)) +
  geom_point(size=2) +
  #geom_smooth(method=lm) +
  geom_text(aes(label=utterance), size=4) +
  facet_grid(.~interpretationKind) +
  theme_bw() +
  xlab("Human") +
  ylab("Model") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=12))
#scale_color_discrete(guide=FALSE)

church.all.comp.exact <- subset(church.all.comp, interpretationKind=="exact")
with(church.all.comp.exact, cor.test(interpretationProb, model))

church.all.comp.fuzzy <- subset(church.all.comp, interpretationKind=="fuzzy")
with(church.all.comp.fuzzy, cor.test(interpretationProb, model))

church.all.comp.hyperbole <- subset(church.all.comp, interpretationKind=="hyperbolic")
with(church.all.comp.hyperbole, cor.test(interpretationProb, model))

#################################
# full human and model comparison (no breakdown into effects)
#################################

church.full.summary <- aggregate(data=c.all, adjustedProb ~ utterance + meaning + domain, FUN=sum)
church.full.summary <- church.full.summary[with(church.full.summary, order(domain, utterance, meaning)),]
church.full.raw.summary <- aggregate(data=c.all, probability ~ utterance + meaning + domain, FUN=sum)
church.full.raw.summary <- church.full.raw.summary[with(church.full.raw.summary, order(domain, utterance, meaning)),]
model.full.raw.summary <- aggregate(data=m.all, probability ~ utterance + meaning + domain, FUN=sum)
model.full.raw.summary <- model.full.raw.summary[with(model.full.raw.summary, order(domain, utterance, meaning)),]

compare.full.summary <- human.full.summary
colnames(church.full.summary)[2] <- "interpretation"
colnames(church.full.summary)[4] <- "model"
colnames(compare.full.summary)[6] <- "human"
compare.full.summary <- join(compare.full.summary, church.full.summary, by=c("domain", "utterance", "interpretation"))
compare.full.summary[is.na(compare.full.summary)] <- 0

#compare.full.summary$model <- church.full.summary$adjustedProb
#compare.full.summary$model.raw <- church.full.raw.summary$probability
#compare.full.summary$oldModel <- model.full.summary$adjustedProb
#compare.full.summary$oldModel.raw <- model.full.raw.summary$probability

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

# compare previous model with current
# no consideration of valence
with(compare.full.summary, cor.test(model, oldModel))
with(compare.full.summary, cor.test(model.raw, oldModel.raw))

# considering valence
c.all <- c.all[with(c.all, order(domain, utterance, meaning, valence)),]
m.all <- m.all[with(m.all, order(domain, utterance, meaning, valence)),]
cor.test(c.all$probability, m.all$probability)
cor.test(c.all$adjustedProb, m.all$adjustedProb)

model.church.comp <- c.all
model.church.comp$oldModel.raw <- m.all$probability

ggplot(model.church.comp, aes(x=probability, y=oldModel.raw, color=interpretationKind, shape=domain)) +
  geom_point() +
  theme_bw() +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14)) +
          xlab("New Church model") +
          ylab("Old model (with lexical uncertainty)")



##############
# plot human and model full distributions in three domains
##############

# multiplot(church.full.plot, human.full.plot, cols=2)
# 
# # add opinion probabilities
# m.all.opinion <- subset(aggregate(data=m.all, adjustedProb ~ domain + utterance + valence, FUN=sum), valence=="2")
# colnames(m.all.opinion)[3] <- "interpretationKind"
# m.all.opinion$interpretationKind <- "expensive"
# 
# m.all.interpKinds.opinion <- rbind(subset(m.all.interpKinds,interpretationKind!="other"), 
#                                    m.all.opinion)
# 
# m.all.interpKinds.opinion$interpretationKind <- 
#   factor(m.all.interpKinds.opinion$interpretationKind, levels=c("exact", "fuzzy", "hyperbolic", "expensive"))
# 
# 
# model.all <- rbind(m.all.interpKinds.opinion, 
#                    data.frame(utterance=c(50,51,50,51,50,51), 
#                               interpretationKind=c("hyperbolic","hyperbolic",
#                                                    "hyperbolic","hyperbolic","hyperbolic","hyperbolic"), 
#                               adjustedProb=c(0,0,0,0,0,0), 
#                               domain=c("laptop","laptop","electric kettle","electric kettle",
#                                        "watch","watch")))
# 
# 
# 
# model.all <- model.all[with(model.all, order(domain, utterance, interpretationKind)),]
# model.all.comp <- 
#   human.all[with(human.all, order(domain, utterance, interpretationKind)), ]
# 
# model.all.comp$model <- model.all$adjustedProb
# model.all.comp.noOpinion <- subset(model.all.comp, interpretationKind != "expensive")

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

with(compare.full.summary, cor.test(human, model))

# stats on effects for humans
compare.full.summary$utteranceType <- ifelse(as.numeric(as.character(compare.full.summary$utterance)) %% 10 == 0, "round",
                                             "sharp")

summary.exact <- subset(compare.full.summary, interpretationKind == "exact")
summary(lm(data=summary.exact, human ~ utteranceType))




summary(lm(data=compare.full.summary, human ~ model))

###################################
# plot bar blots of effects for model and human, per domain
###################################
# set domain
dom <- "laptop"
# convert to long form
church.dom <- subset(church.all.comp, domain==dom)
church.dom$interpretationProb <- church.dom$model
church.dom$kind <- "model"
human.dom <- subset(model.all.comp.no_opinion, domain==dom)
human.dom$kind <- "human"
model.dom.comp <- rbind(model.dom, human.dom)

levels(church.all.comp$interpretationKind)<- c("Exact", "Fuzzy", "Hyperbolic", "Affective")
levels(church.all.comp$domain) <- c("Electric Kettle", "Laptop", "Watch")

church.all.comp.noOpinion <- subset(church.all.comp, interpretationKind != "Affective")

## church model
ggplot(church.all.comp.noOpinion, aes(x=utterance, y=model, fill=interpretationKind)) +
  geom_bar(stat="identity", color="black") +
  #geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2) +
  facet_grid(interpretationKind ~ domain) +
  theme_bw() +
  scale_fill_brewer(guide=FALSE, palette="Accent") +
  theme(strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14, angle=-90, hjust=-0.01,vjust=0.3), axis.title.x=element_text(size=16)) +
  xlab("Utterance") +
  ylab("Probability")

## human
ggplot(church.all.comp.noOpinion, aes(x=utterance, y=interpretationProb, fill=interpretationKind)) +
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=.2) +
  facet_grid(interpretationKind ~ domain) +
  theme_bw() +
  scale_fill_brewer(guide=FALSE, palette="Accent") +
  theme(strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14, angle=-90, hjust=-0.01,vjust=0.3), axis.title.x=element_text(size=16)) +
          xlab("Utterance") +
          ylab("Probability")

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

#######
# affect pairs
#######

ap <- read.table("../../data/mTurkExp/hyperboleThreeDomains/affect_pairs2_long.csv", 
                strip.white=TRUE, header=TRUE, sep=",")

ap$actualPriceSharpened <- ifelse(ap$actualType=="sharp", ap$actualPriceRounded+1, ap$actualPriceRounded)
ap$utteredPriceSharpened <- ifelse(ap$utteredType=="sharp", ap$utteredPriceRounded+1, ap$utteredPriceRounded)
ap$actualPriceRounded <- factor(ap$actualPriceRounded)
ap$utteredPriceRounded <- factor(ap$utteredPriceRounded)
ap$actualPriceSharpened <- factor(ap$actualPriceSharpened)
ap$utteredPriceSharpened <- factor(ap$utteredPriceSharpened)

ap$isHyperbole <- ifelse(ap$actualPriceRounded!=ap$utteredPriceRounded,
                                 "hyperbole", "literal")

summary(lm(data=ap, probOpinion ~ isHyperbole))

ap.summary <- summarySE(ap, measurevar="probOpinion", 
                        groupvars=c("actualPriceRounded", "isHyperbole", "domain"))
                        #groupvars=c("actualPriceRounded", "isHyperbole"))
ggplot(ap.summary, aes(x=actualPriceRounded, y=probOpinion, group=isHyperbole, color=domain, shape=isHyperbole, linetype=isHyperbole)) +
  geom_point(size=3) +
  geom_line() +
  #geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), width=0.2) +
  facet_grid(domain ~ .) +
  theme_bw() +
  theme(axis.text.x  = element_text(size=10, angle=-90)) +
  scale_shape_manual(values=c(15, 6)) +
  scale_color_brewer(palette="Accent", guide=FALSE)

#################################
# Model affect
#################################
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


ap.summary$type <- "Human"

church.full.affect.summary <- church.affect.summary
colnames(church.full.affect.summary)[5] <- "probOpinion"
colnames(church.full.affect.summary)[2] <- "actualPriceRounded"
church.full.affect.summary$type <- "Full model"

church.noAffectPrior.affect.summary <- church.affect.summary
colnames(church.noAffectPrior.affect.summary)[5] <- "probOpinion"
colnames(church.noAffectPrior.affect.summary)[2] <- "actualPriceRounded"
church.noAffectPrior.affect.summary$type <- "Uniform affect prior"

comp.affect.summary <- rbind(ap.summary, church.full.affect.summary, church.noAffectPrior.affect.summary)
comp.affect.summary$type <- factor(comp.affect.summary$type, levels=c("Human", "Full model", "Uniform affect prior"))
colnames(comp.affect.summary)[2] <- "Literalness"

ggplot(comp.affect.summary, aes(x=actualPriceRounded, y=probOpinion, group=Literalness, color=domain, shape=Literalness, linetype=Literalness)) +
  geom_point(size=3) +
  geom_line() +
  #geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), width=0.2) +
  facet_grid(domain ~ type) +
  theme_bw() +
  xlab("Price state rounded") +
  ylab("P(affect | utterance and price state)") +
  scale_shape_manual(values=c(16, 6)) +
  scale_color_brewer(palette="Accent", guide=FALSE) +
  theme(axis.title.x=element_text(size=16), axis.text.x=element_text(size=14, angle=-90),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        legend.title=element_text(size=0), legend.text=element_text(size=14),
        legend.position=c(0.9, 0.9))
  




####################

uttered.summary <- summarySE(ap, measurevar="probOpinion",
                             groupvars=c("domain", "utteredPriceRounded"))
uttered.summary$actualPriceRounded <- "utteredPrice"
uttered.summary$color<-"gray"

actual.summary <- summarySE(ap, measurevar="probOpinion",
                            groupvars=c("domain", "actualPriceRounded"))
colnames(actual.summary)[2] <- "utteredPriceRounded"
actual.summary$actualPriceRounded <- "actualPrice"
actual.summary$color <-"dark gray"

ap.summary <- summarySE(ap, measurevar="probOpinion", 
                        groupvars=c("actualPriceRounded", "utteredPriceRounded", "domain"))
ap.summary$color <- "red"

combined <- rbind(ap.summary, actual.summary, uttered.summary)

ggplot(combined, aes(x=utteredPriceRounded, y=probOpinion, fill=color)) +
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), width=0.2) +
  facet_grid(domain ~ actualPriceRounded) +
  theme_bw() +
  scale_fill_manual(values=c("gray", "white", "#FF9999"), guide=FALSE) +
  xlab("Uttered Price") +
  ylab("Probability of opinion")

# only look at cases where the actual price is 50 in the kettle domain
ap.50 <- subset(ap, actualPriceRounded=="50" & domain=="electric kettle")
ggplot(ap, aes(x=utteredPriceRounded, y=probOpinion)) +
  geom_point() +
  facet_grid(.~actualPriceRounded) +
  theme_bw()

ap.domain <-subset(ap, domain=="electric kettle")
ap.domain$workerID.number <- as.numeric(ap.domain$workerID)

ap.domain.small <- subset(ap.domain, workerID.number > 0)

ggplot(ap.domain.small, aes(x=utteredPriceRounded, y=probOpinion)) +
  geom_point() +
  facet_grid(.~actualPriceRounded) +
  theme_bw()

ap.domain.small.summary <-summarySE(ap.domain.small, measurevar="probOpinion", 
                          groupvars=c("actualPriceRounded", "utteredPriceRounded"))

ggplot(ap.domain.small.summary, aes(x=utteredPriceRounded, y=probOpinion, fill=actualPriceRounded)) +
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), width=0.2) +
  facet_grid(.~actualPriceRounded) +
  theme_bw() +
  scale_fill_discrete(guide=FALSE)
  

