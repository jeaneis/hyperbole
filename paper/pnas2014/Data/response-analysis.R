source("../summarySE.R")
library(ggplot2)

#########################################
# Experiment 1
#########################################
exp1 <- read.csv("http://stanford.edu/~justinek/hyperbole-paper/data/experiment1-normalized.csv")
# Code interpretation kind
exp1$interpretationKind <-
  ifelse(as.numeric(exp1$utteranceRounded) > as.numeric(exp1$stateRounded), "hyperbolic",
         ifelse(exp1$utterance==exp1$state, "exact",
                ifelse(exp1$utteranceRounded==exp1$stateRounded, "fuzzy", "other")))
exp1$utterance <- factor(exp1$utterance)
exp1$state <- factor(exp1$state)
exp1$utteranceRounded <- factor(exp1$utteranceRounded)
exp1$stateRounded <- factor(exp1$stateRounded)

exp1.summary <- summarySE(exp1, measurevar="stateProb", 
                          groupvars=c("domain", "utterance", "utteranceType", "utteranceRounded",
                                      "state", "stateType", "stateRounded", "interpretationKind"))

#########
# Fig.S2
#########
#   figureS2 <- ggplot(exp1.summary, aes(x=state, y=stateProb)) +
#     geom_bar(stat="identity", color="black", fill="#FF9999") +
#     geom_errorbar(aes(ymin=stateProb-se, ymax=stateProb+se),width=0.2) +
#     facet_grid(domain ~ utterance) +
#     theme_bw() +
#     ylab("Probability") +
#     xlab("Interpretation") +
#     #ggtitle("Human") +
#     theme(axis.title.x = element_text(size=14),
#           axis.text.x  = element_text(size=6, angle=-90),
#           axis.title.y = element_text(size=14),
#           axis.text.y = element_text(size=14))
#   


#########################################
# Model prdictions
#########################################
#model <- read.csv("http://stanford.edu/~justinek/hyperbole-paper/data/model-predictions.csv")
#model <- read.csv("Model/FullModel/predictions.csv")
bestFit = 0
bestCost = 0
bestAlpha = 0
bestModel <- data.frame()
for (cost in seq(from=0.11, to=0.37, by=0.01)) {
  #model <- read.csv("/Users/justinek/Documents/Grad_school/Research/Hyperbole/hyperbole_github/code/model/PNASModel/ParsedOutput/output-0.11.txt")
  model <- read.csv(paste("/Users/justinek/Documents/Grad_school/Research/Hyperbole/hyperbole_github/code/model/PNASModel/ParsedOutput/output-", 
                          cost, ".txt", sep=""))
  
  # Code "rounded" version of utterance and state
  model$utteranceRounded <- (floor(model$utterance/10))*10
  model$stateRounded <- (floor(model$state/10)) * 10
  model$utterance <- factor(model$utterance)
  model$state <- factor(model$state)
  model$affect <- factor(model$affect)
  model$utteranceRounded <- factor(model$utteranceRounded)
  model$stateRounded <- factor(model$stateRounded)
  # Code interpretation kind
  model$interpretationKind <- 
    ifelse(as.numeric(model$utteranceRounded) > as.numeric(model$stateRounded), "hyperbolic", 
           ifelse(model$utterance==model$state, "exact",
                  ifelse(model$utteranceRounded==model$stateRounded, "fuzzy", "other")))
  
  ###########################
  # Fit Luce choice parameter
  ###########################
  i = 0.36
  #for (i in seq(from=0.1, to=0.4, by=0.01)) {
    model.fit <- model
    model.fit$raisedProb <- model.fit$probability^i
    normalizingFactors <- aggregate(data=model.fit, raisedProb ~ domain + utterance, FUN=sum)
    colnames(normalizingFactors)[3] <- "normalizing"
    model.fit <- join(model.fit, normalizingFactors, by=c("domain", "utterance"))
    model.fit$adjustedProb <- model.fit$raisedProb / model.fit$normalizing
    # Marginalize over affect
    model.fit.state <- aggregate(data=model.fit, adjustedProb ~ 
                                   utterance + state + domain + utteranceRounded +
                                   stateRounded + interpretationKind, FUN=sum)
    
    exp1.exact <- subset(exp1, interpretationKind=="exact")
    exp1.exact.agg <- aggregate(data=exp1.exact, 
                                stateProb ~ utterance + utteranceType + utteranceRounded + domain + workerID, FUN=sum)
    exp1.fuzzy <- subset(exp1, interpretationKind=="fuzzy")
    exp1.fuzzy.agg <- aggregate(data=exp1.fuzzy,
                                stateProb ~ utterance + utteranceType + utteranceRounded + domain + workerID, FUN=sum)
    
    exp1.exact.summary <- aggregate(data=exp1.exact.agg, stateProb ~ 
                                      utterance + utteranceType + utteranceRounded + domain, FUN=mean)
    exp1.fuzzy.summary <- aggregate(data=exp1.fuzzy.agg, stateProb ~ 
                                      utterance + utteranceType + utteranceRounded + domain, FUN=mean)
    colnames(exp1.exact.summary)[5] <- "probExact"
    colnames(exp1.fuzzy.summary)[5] <- "probFuzzy"
    exp1.halo <- join(exp1.exact.summary, exp1.fuzzy.summary, by=c("utterance", "utteranceType", "utteranceRounded", "domain"))
    exp1.halo$humanHalo <- exp1.halo$probExact - exp1.halo$probFuzzy
    
    model.exact <- subset(model.fit.state, interpretationKind=="exact")
    model.fuzzy <- subset(model.fit.state, interpretationKind=="fuzzy")
    colnames(model.exact)[7] <- "probExact"
    colnames(model.fuzzy)[7] <- "probFuzzy"
    
    model.halo <- join(model.exact, model.fuzzy, by=c("domain", "utterance", "utteranceRounded",
                                                      "stateRounded"))
    model.halo$utteranceType <- ifelse(as.numeric(as.character(model.halo$utterance))==
                                         as.numeric(as.character(model.halo$utteranceRounded)), "round", "sharp")
    model.halo$modelHalo <- model.halo$probExact - model.halo$probFuzzy
    
    compare.halo <- join(exp1.halo, model.halo, by=c("utterance", "domain", "utteranceRounded"))
    
    r <- with(compare.halo, cor(humanHalo, modelHalo))
    if (r > bestFit) {
      bestFit <- r
      bestAlpha <- i
      bestCost <- cost
      bestModel <- model
    }
  #}
}

##########################
# Compute whether the first 1/3 of the experiment has more of a halo effect than the last 1/3
##########################
exp1.first <- subset(exp1, order <= 5)
exp1.last <- subset(exp1, order >= 10)

computeHalo <- function(exp1) {
  exp1.exact <- subset(exp1, interpretationKind=="exact")
  exp1.exact.agg <- aggregate(data=exp1.exact, 
                              stateProb ~ utterance + utteranceType + utteranceRounded + domain + workerID, FUN=sum)
  exp1.fuzzy <- subset(exp1, interpretationKind=="fuzzy")
  exp1.fuzzy.agg <- aggregate(data=exp1.fuzzy,
                              stateProb ~ utterance + utteranceType + utteranceRounded + domain + workerID, FUN=sum)
  
  exp1.exact.summary <- aggregate(data=exp1.exact.agg, stateProb ~ 
                                    utterance + utteranceType + utteranceRounded + domain, FUN=mean)
  exp1.fuzzy.summary <- aggregate(data=exp1.fuzzy.agg, stateProb ~ 
                                    utterance + utteranceType + utteranceRounded + domain, FUN=mean)
  colnames(exp1.exact.summary)[5] <- "probExact"
  colnames(exp1.fuzzy.summary)[5] <- "probFuzzy"
  exp1.halo <- join(exp1.exact.summary, exp1.fuzzy.summary, by=c("utterance", "utteranceType", "utteranceRounded", "domain"))
  exp1.halo$humanHalo <- exp1.halo$probExact - exp1.halo$probFuzzy
  return(exp1.halo)
}

exp1.first.halo <- computeHalo(exp1.first)
exp1.last.halo <- computeHalo(exp1.last)

colnames(exp1.first.halo)[5] <- "firstProbExact"
colnames(exp1.first.halo)[6] <- "firstProbFuzzy"
colnames(exp1.last.halo)[5] <- "lastProbExact"
colnames(exp1.last.halo)[6] <- "lastProbFuzzy"
colnames(exp1.first.halo)[7] <- "firstHalo"
colnames(exp1.last.halo)[7] <- "lastHalo"

order.compare.halo <- join(exp1.first.halo, exp1.last.halo, by=c("utterance", "utteranceType",
                                                                 "utteranceRounded", "domain"))
exp1.first.summary <- summarySE(exp1.first, measurevar="stateProb", 
                          groupvars=c("domain", "utterance", "utteranceType", "utteranceRounded",
                                      "state", "stateType", "stateRounded", "interpretationKind"))

exp1.last.summary <- summarySE(exp1.last, measurevar="stateProb", 
                                groupvars=c("domain", "utterance", "utteranceType", "utteranceRounded",
                                            "state", "stateType", "stateRounded", "interpretationKind"))

with(order.compare.halo, t.test(firstHalo, lastHalo, paired=TRUE))
with(order.compare.halo, t.test(firstProbExact, lastProbExact))
with(order.compare.halo, t.test(firstProbFuzzy, lastProbFuzzy))
with(subset(order.compare.halo, utteranceType=="round"), t.test(firstHalo, lastHalo))
with(subset(order.compare.halo, utteranceType=="sharp"), t.test(firstHalo, lastHalo))

##########################
# Compute model predictions with best parameter fit
##########################
model <- bestModel 
model$raisedProb <- model$probability^bestAlpha
normalizingFactors <- aggregate(data=model, raisedProb ~ domain + utterance, FUN=sum)
colnames(normalizingFactors)[3] <- "normalizing"
model <- join(model, normalizingFactors, by=c("domain", "utterance"))
model$modelProb <- model$raisedProb / model$normalizing

# Marginalize over affect
model.state <- aggregate(data=model, modelProb ~ 
                           domain + utterance + state + utteranceRounded + 
                           stateRounded + interpretationKind, FUN=sum)

#######
# Fig.S1
#######
figureS1 <- ggplot(model.state, aes(x=state, y=modelProb)) +
  geom_bar(stat="identity", color="black", fill="gray") +
  facet_grid(domain ~ utterance) +
  theme_bw() +
  ylab("Probability") +
  xlab("Interpretation") +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=6, angle=-90),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14))

######
# Fig. 1: interpretation kinds (Exact, Fuzzy, Hyperbolic, Affective)
######
my.colors.effects <- c("#6699CC", "#66CC99", "#FF9966", "#FFCCCC")
# Aggregate model over valence and different ways to be hyperbolic
model.stateKinds <- aggregate(data=model, modelProb ~ domain + utterance + interpretationKind,
                              FUN=sum)
model.affect <- subset(aggregate(data=model, modelProb ~ domain + utterance + affect, FUN=sum), affect=="1")
colnames(model.affect)[3] <- "interpretationKind"
model.affect$interpretationKind <- "affective"
model.effects <- rbind(subset(model.stateKinds,interpretationKind!="other"), model.affect)
model.effects$interpretationKind <- 
  factor(model.effects$interpretationKind, levels=c("exact", "fuzzy", "hyperbolic", "affective"))
model.effects <- rbind(model.effects, 
                       data.frame(utterance=c(50,51,50,51,50,51), 
                                  interpretationKind=c("hyperbolic","hyperbolic",
                                                       "hyperbolic","hyperbolic","hyperbolic","hyperbolic"), 
                                  modelProb=c(0,0,0,0,0,0), 
                                  domain=c("laptop","laptop","electric kettle","electric kettle",
                                           "watch","watch")))

levels(model.effects$interpretationKind)<- c("Exact", "Fuzzy", "Hyperbolic", "Affective")
levels(model.effects$domain) <- c("Electric Kettle", "Laptop", "Watch")

figure1 <- ggplot(model.effects, aes(x=utterance, y=modelProb, fill=interpretationKind)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(interpretationKind ~ domain) +
  theme_bw() +
  scale_fill_manual(guide=FALSE, values=my.colors.effects) +
  theme(strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14, angle=-90, hjust=-0.01,vjust=0.3), axis.title.x=element_text(size=16)) +
  xlab("Utterance") +
  ylab("Probability")

ggplot(subset(model.effects, interpretationKind=="Exact" | interpretationKind=="Fuzzy"), 
       aes(x=utterance, y=modelProb, fill=interpretationKind)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(interpretationKind ~ domain) +
  theme_bw() +
  scale_fill_manual(guide=FALSE, values=my.colors.effects) +
  theme(strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14, angle=-90, hjust=-0.01,vjust=0.3), axis.title.x=element_text(size=16)) +
  xlab("Utterance") +
  ylab("Probability")

ggplot(subset(exp1.last.summary, interpretationKind=="exact" | interpretationKind=="fuzzy"), 
       aes(x=utterance, y=stateProb, fill=interpretationKind)) +
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=stateProb-se, ymax=stateProb+se), width=0.2) +
  facet_grid(interpretationKind ~ domain) +
  theme_bw() +
  scale_fill_manual(guide=FALSE, values=my.colors.effects) +
  theme(strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14, angle=-90, hjust=-0.01,vjust=0.3), axis.title.x=element_text(size=16)) +
  xlab("Utterance") +
  ylab("Probability")


##########
# Fig. 2(A): Model predictions versus average human responses
##########
my.colors.4effects <- c("#003399", "#339966", "#FF6633", "grey")
compare <- join(exp1.summary, model.state, by=c("utterance", "state", "domain"))
figure2a <- ggplot(compare, aes(x=modelProb, y=stateProb)) +
  geom_point(size=2.5, aes(x=modelProb, y=stateProb, color=interpretationKind, shape=domain)) +
  geom_smooth(data=compare, aes(x=modelProb, y=stateProb), method=lm, color="black", linetype=2) +
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
  scale_x_continuous(expand=c(0, 0), limits=c(0,0.4)) +
  scale_y_continuous(expand=c(0, 0)) +
  expand_limits(y=0,x=0) +
  scale_color_manual(values=my.colors.4effects, name="Interpretation Kind", 
                     breaks=c("exact", "fuzzy", "hyperbolic", "other"),
                     labels=c("Exact", "Fuzzy", "Hyperbolic", "Other"))+
  scale_shape_discrete(name="Item", breaks=c("electric kettle", "laptop", "watch"),
                       labels=c("Electric Kettle", "Laptop", "Watch"))

with(compare, cor.test(modelProb, stateProb)) 

#################
# Make comparison models
#################
makeModel <- function(filename){
  m <- read.csv(filename)
  # Code "rounded" version of utterance and state
  m$utteranceRounded <- (floor(m$utterance/10))*10
  m$stateRounded <- (floor(m$state/10)) * 10
  m$utterance <- factor(m$utterance)
  m$state <- factor(m$state)
  m$affect <- factor(m$affect)
  m$utteranceRounded <- factor(m$utteranceRounded)
  m$stateRounded <- factor(m$stateRounded)
  # Code interpretation kind
  m$interpretationKind <- 
    ifelse(as.numeric(m$utteranceRounded) > as.numeric(m$stateRounded), "hyperbolic", 
           ifelse(m$utterance==m$state, "exact",
                  ifelse(m$utteranceRounded==m$stateRounded, "fuzzy", "other")))
  m$raisedProb <- m$probability^bestAlpha
  normalizingFactors <- aggregate(data=m, raisedProb ~ domain + utterance, FUN=sum)
  colnames(normalizingFactors)[3] <- "normalizing"
  m <- join(m, normalizingFactors, by=c("domain", "utterance"))
  m$modelProb <- m$raisedProb / m$normalizing
  # Marginalize over affect
  m.state <- aggregate(data=m, modelProb ~ 
                         domain + utterance + state + utteranceRounded + stateRounded + interpretationKind, FUN=sum)
  return(m.state)
}

makeModelKeepAffect <- function(filename){
  m <- read.csv(filename)
  # Code "rounded" version of utterance and state
  m$utteranceRounded <- (floor(m$utterance/10))*10
  m$stateRounded <- (floor(m$state/10)) * 10
  m$utterance <- factor(m$utterance)
  m$state <- factor(m$state)
  m$affect <- factor(m$affect)
  m$utteranceRounded <- factor(m$utteranceRounded)
  m$stateRounded <- factor(m$stateRounded)
  # Code interpretation kind
  m$interpretationKind <- 
    ifelse(as.numeric(m$utteranceRounded) > as.numeric(m$stateRounded), "hyperbolic", 
           ifelse(m$utterance==m$state, "exact",
                  ifelse(m$utteranceRounded==m$stateRounded, "fuzzy", "other")))
  m$raisedProb <- m$probability^bestAlpha
  normalizingFactors <- aggregate(data=m, raisedProb ~ domain + utterance, FUN=sum)
  colnames(normalizingFactors)[3] <- "normalizing"
  m <- join(m, normalizingFactors, by=c("domain", "utterance"))
  m$modelProb <- m$raisedProb / m$normalizing
  return(m)
}

#################
# Fig. 2(B): Goals
#################

#model.noAffectGoal <- makeModel("http://stanford.edu/~justinek/hyperbole-paper/data/model-predictions-noAffectGoal")
#model.noImpreciseGoal <- makeModel("http://stanford.edu/~justinek/hyperbole-paper/data/model-predictions-noImpreciseGoal")
model.noAffectGoal <- makeModel("Model/JustStateGoals/predictions.csv")
model.noImpreciseGoal <- makeModel("Model/JustPreciseAndAffectGoals/predictions.csv")
this.domain <- "electric kettle"
this.utterance <- "1000"
model.literal.example <- data.frame(utterance=c(this.utterance), state=c(this.utterance), domain=c(this.domain),
                                    utteranceRounded=c(this.utterance), stateRounded=c(this.utterance), 
                                    modelProb=c(1), se=c(0), type=c("No goals"), interpretationKind=c("exact"))
model.noAffectGoal.example <- subset(model.noAffectGoal, domain==this.domain & utterance==this.utterance)
model.noImpreciseGoal.example <- subset(model.noImpreciseGoal, domain==this.domain & utterance==this.utterance)
model.allGoals.example <- subset(model.state, domain==this.domain & utterance==this.utterance)
human.example <- subset(exp1.summary, domain==this.domain & utterance==this.utterance)

model.noAffectGoal.example$type <- "Imprecise goal"
model.noAffectGoal.example$se <- 0
model.noImpreciseGoal.example$type <- "Affect goal"
model.noImpreciseGoal.example$se <- 0
model.allGoals.example$type <- "Full model"
model.allGoals.example$se <- 0
human.example$type <- "Human"
human.example$utteranceType <- NULL
human.example$stateType <- NULL
human.example$N <- NULL
human.example$sd <- NULL
human.example$ci <- NULL
colnames(human.example)[7] <- "modelProb"

goals.comparison <- rbind(model.literal.example, model.noAffectGoal.example,
                          model.noImpreciseGoal.example, model.allGoals.example, human.example)
goals.comparison$state <- factor(goals.comparison$state, 
                                 levels=c("50","51","500","501","1000","1001","5000","5001","10000","10001"))

#### colors for paper
my.colors.goals<- c("#d9d9d9", "#dadaeb", "#bcbddc", "#9edae5", "#17becf")
#### colors for presentation
# my.colors.goals <- c("#d9d9d9", "#dadaeb", "#bcbddc", "#cc6699", "#ff9896")
figure2b <- ggplot(goals.comparison, aes(x=state, y=modelProb, fill=type)) +
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=modelProb-se, ymax=modelProb+se), width=0.2) +
  theme_bw() +
  facet_grid(.~type) +
  scale_fill_manual(values=my.colors.goals, guide=FALSE) +
  ggtitle('"The electric kettle cost 1,000 dollars."') +
  xlab("Interpretation") +
  ylab("Probability") +
  theme(axis.title.x=element_text(size=16), axis.text.x=element_text(size=14, angle=-90, vjust=0.3),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), strip.text.y=element_text(size=16),
        legend.text=element_text(size=0),title=element_text(size=18)) +
  scale_y_continuous(expand=c(0, 0))

##############
# Figure 3A: Uniform priors
##############
exp1.hyperbole <- subset(exp1, interpretationKind=="hyperbolic")
exp1.hyperbole.agg <- aggregate(data=exp1.hyperbole, 
                                stateProb ~ utterance + utteranceType + utteranceRounded + domain + workerID, FUN=sum)
exp1.hyperbole.summary <- summarySE(exp1.hyperbole.agg, measurevar="stateProb", 
                                    groupvars=c("utteranceRounded", "domain"))

model.hyperbole <- subset(model.state, interpretationKind=="hyperbolic")
model.hyperbole.agg <- aggregate(data=model.hyperbole, modelProb ~ utterance + domain + utteranceRounded, FUN=sum)
model.hyperbole.agg$domain <- factor(model.hyperbole.agg$domain)
model.hyperbole.summary <- summarySE(model.hyperbole.agg, measurevar="modelProb",
                                     groupvars=c("utteranceRounded", "domain"))

#model.noPriors <- makeModel("http://stanford.edu/~justinek/hyperbole-paper/data/model-predictions-noPrior.csv")
model.noPriors <- makeModel("Model/UniformPriors/predictions.csv")
model.noPriors.hyperbole <- subset(model.noPriors, interpretationKind=="hyperbolic")
model.noPriors.hyperbole.agg <- aggregate(data=model.noPriors.hyperbole,
                                          modelProb ~ utterance + domain + utteranceRounded, FUN=sum)
model.noPriors.hyperbole.agg$domain <- factor(model.noPriors.hyperbole.agg$domain)
model.noPriors.hyperbole.summary <- summarySE(model.noPriors.hyperbole.agg, measurevar="modelProb",
                                              groupvars=c("utteranceRounded", "domain"))

exp1.hyperbole.summary$type <- "Human"
colnames(exp1.hyperbole.summary)[4] <- "modelProb"
model.hyperbole.summary$type <- "Full model"
model.noPriors.hyperbole.summary$type <- "Uniform price prior"

priors.comparison <- rbind(exp1.hyperbole.summary, model.hyperbole.summary, model.noPriors.hyperbole.summary)
priors.comparison$type <- factor(priors.comparison$type, levels=c("Human", "Full model", "Uniform price prior"))
my.colors.domains <- c("#ff9896", "#17becf", "#e7ba52")
figure3a <- ggplot(priors.comparison, aes(x=utteranceRounded, y=modelProb, group=domain, color=domain)) +
  geom_point(size=5) +
  geom_line(aes(color=domain), size=1) +
  #geom_text(aes(label=utterance, color=domain)) +
  facet_grid(.~type) +
  geom_errorbar(aes(ymin=modelProb-se, ymax=modelProb+se), width=0.2, color="grey") +
  theme_bw() +
  xlab("Utterance rounded") +
  ylab("P(hyperbole)") +
  theme(legend.title=element_text(size=0), legend.position=c(0.8, 0.85),
        axis.title.x=element_text(size=16), axis.text.x=element_text(size=14, angle=-90),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.x=element_text(size=16), legend.text=element_text(size=14)) +
  scale_color_manual(values=my.colors.domains)

#################
# Fig. 3(B): Uniform utterance costs
#################

exp1.exact <- subset(exp1, interpretationKind=="exact")
exp1.exact.agg <- aggregate(data=exp1.exact, 
                            stateProb ~ utterance + utteranceType + utteranceRounded + domain + workerID, FUN=sum)
exp1.fuzzy <- subset(exp1, interpretationKind=="fuzzy")
exp1.fuzzy.agg <- aggregate(data=exp1.fuzzy,
                            stateProb ~ utterance + utteranceType + utteranceRounded + domain + workerID, FUN=sum)

exp1.exact.summary <- aggregate(data=exp1.exact.agg, stateProb ~ 
                                  utterance + utteranceType + utteranceRounded + domain, FUN=mean)
exp1.fuzzy.summary <- aggregate(data=exp1.fuzzy.agg, stateProb ~ 
                                  utterance + utteranceType + utteranceRounded + domain, FUN=mean)
colnames(exp1.exact.summary)[5] <- "probExact"
colnames(exp1.fuzzy.summary)[5] <- "probFuzzy"
exp1.halo <- join(exp1.exact.summary, exp1.fuzzy.summary, by=c("utterance", "utteranceType", "utteranceRounded", "domain"))
exp1.halo$halo <- exp1.halo$probExact - exp1.halo$probFuzzy
exp1.halo.summary <- summarySE(exp1.halo, measurevar="halo", groupvars=c("utteranceType", "domain"))

model.exact <- subset(model.state, interpretationKind=="exact")
model.fuzzy <- subset(model.state, interpretationKind=="fuzzy")
colnames(model.exact)[7] <- "probExact"
colnames(model.fuzzy)[7] <- "probFuzzy"

model.halo <- join(model.exact, model.fuzzy, by=c("domain", "utterance", "utteranceRounded",
                                                  "stateRounded"))
model.halo$utteranceType <- ifelse(as.numeric(as.character(model.halo$utterance))==
                                     as.numeric(as.character(model.halo$utteranceRounded)), "round", "sharp")
model.halo$halo <- model.halo$probExact - model.halo$probFuzzy
model.halo.summary <- summarySE(model.halo, measurevar="halo", groupvars=c("utteranceType", "domain"))

model.noCost <- makeModel("Model/UniformCost/predictions.csv")
model.noCost.agg <- aggregate(data=model.noCost,
                              modelProb ~ domain + utterance + utteranceRounded + 
                                state + stateRounded + interpretationKind, FUN=sum)
model.noCost.exact <- subset(model.noCost.agg, interpretationKind=="exact")
model.noCost.fuzzy <- subset(model.noCost.agg, interpretationKind=="fuzzy")
colnames(model.noCost.exact)[7] <- "probExact"
colnames(model.noCost.fuzzy)[7] <- "probFuzzy"
model.noCost.halo <- join(model.noCost.exact, model.noCost.fuzzy, by=c("domain", "utterance",
                                                                       "utteranceRounded", "stateRounded"))
model.noCost.halo$halo <- model.noCost.halo$probExact - model.noCost.halo$probFuzzy
model.noCost.halo$utteranceType <- ifelse(as.numeric(as.character(model.noCost.halo$utterance))==
                                            as.numeric(as.character(model.noCost.halo$utteranceRounded)), "round", "sharp")

model.noCost.halo.summary <- summarySE(model.noCost.halo, measurevar="halo", groupvars=c("utteranceType", "domain"))

exp1.halo.summary$type <- "Human"
model.halo.summary$type <- "Full model"
model.noCost.halo.summary$type <- "Uniform utterance cost"

cost.comparison <- rbind(exp1.halo.summary, model.halo.summary, model.noCost.halo.summary)
cost.comparison$type <- factor(cost.comparison$type, levels=c("Human", "Full model", "Uniform utterance cost"))
figure3b <- ggplot(cost.comparison, aes(x=utteranceType, y=halo, group=domain, color=domain, shape=utteranceType)) +
  geom_point(size=5) +
  #geom_bar(color="black", stat="identity", position=position_dodge()) +
  geom_line(linetype=2, size=1) +
  geom_errorbar(aes(ymin=halo-se, ymax=halo+se), width=0.1, color="grey")+
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


#################
# Fig. 4(A): Affect scatter plot
#################

exp2 <- read.table("http://stanford.edu/~justinek/hyperbole-paper/data/experiment2-raw.csv", 
                   strip.white=TRUE, header=TRUE, sep=",")
exp2$isHyperbole <- ifelse(exp2$utteranceRounded > exp2$stateRounded,
                           "hyperbolic", "literal")

exp2.summary <- summarySE(exp2, measurevar="affectProb", 
                          groupvars=c("utteranceRounded", "stateRounded", "isHyperbole", "domain"))

# only consider utterance/state pairs where utterance >= state
model.affect <- subset(model, as.numeric(as.character(utteranceRounded)) >= 
                         as.numeric(as.character(stateRounded)) & affect=="1")
colnames(model.affect)[11] <- "probOfAffect"
model.affect <- join(model.affect, model.state, by=c("domain", "utterance", "state", "utteranceRounded",
                                                     "stateRounded", "interpretationKind"))

model.affect$modelAffectProb <- model.affect$probOfAffect / model.affect$modelProb

model.affect$isHyperbole <- ifelse(as.numeric(as.character(model.affect$utteranceRounded)) >
                                     as.numeric(as.character(model.affect$stateRounded)),
                                   "hyperbolic", "literal")
exp2.summary$utteranceRounded <- factor(exp2.summary$utteranceRounded)
exp2.summary$stateRounded <- factor(exp2.summary$stateRounded)
affect.compare <- join(exp2.summary, model.affect, by=c("domain", "utteranceRounded", 
                                                        "stateRounded", "isHyperbole"))

ggplot(affect.compare, aes(x=modelAffectProb, y=affectProb)) +
  #geom_text(aes(label=label), color="dark grey") +
  geom_errorbar(aes(ymin=affectProb-se, ymax=affectProb+se), width=0.01, color="gray") +
  geom_point(aes(x=modelAffectProb, y=affectProb, color=isHyperbole, shape=domain), size=3) +
  geom_smooth(data=affect.compare, aes(x=modelAffectProb, y=affectProb), method=lm, color="black", linetype=2) +
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

with(affect.compare, cor.test(modelAffectProb, affectProb))

#################
# Fig. 4(B): Affect priors comparison
#################

model.affect.givenState <- summarySE(model.affect, measurevar="modelAffectProb",
                                     groupvars=c("stateRounded", "isHyperbole", "domain"))

exp2.affect.givenState <- summarySE(exp2.summary, measurevar="affectProb",
                                    groupvars=c("stateRounded", "isHyperbole", "domain"))

model.noAffectPrior <- makeModelKeepAffect("Model/UniformAffectPriors/predictions.csv")
model.noAffectPrior.state <- makeModel("Model/UniformAffectPriors/predictions.csv")

model.noAffectPrior.affect <- subset(model.noAffectPrior, as.numeric(as.character(utteranceRounded)) >= 
                                       as.numeric(as.character(stateRounded)) & affect=="1")


colnames(model.noAffectPrior.affect)[11] <- "probOfAffect"
model.noAffectPrior.affect <- join(model.noAffectPrior.affect,
                                   model.noAffectPrior.state, by=c("domain", "utterance", "state", "utteranceRounded",
                                                                   "stateRounded", "interpretationKind"))

model.noAffectPrior.affect$modelAffectProb <- model.noAffectPrior.affect$probOfAffect / model.noAffectPrior.affect$modelProb

model.noAffectPrior.affect$isHyperbole <- ifelse(as.numeric(as.character(model.noAffectPrior.affect$utteranceRounded)) >
                                                   as.numeric(as.character(model.noAffectPrior.affect$stateRounded)),
                                                 "hyperbolic", "literal")

model.noAffectPrior.affect.givenState <- summarySE(model.noAffectPrior.affect, measurevar="modelAffectProb",
                                                   groupvars=c("stateRounded", "isHyperbole", "domain"))

exp2.affect.givenState$type <- "Human"
model.affect.givenState$type <- "Full model"
model.noAffectPrior.affect.givenState$type <- "Uniform affect prior"

colnames(model.affect.givenState)[5] <- "affectProb"
colnames(model.noAffectPrior.affect.givenState)[5] <- "affectProb"

affect.compare.priors <- rbind(exp2.affect.givenState,
                               model.affect.givenState, model.noAffectPrior.affect.givenState)

affect.compare.priors$type <- factor(affect.compare.priors$type, levels=c("Human", "Full model", "Uniform affect prior"))
ggplot(affect.compare.priors, aes(x=stateRounded, y=affectProb, group=isHyperbole, color=domain, shape=isHyperbole, linetype=isHyperbole)) +
  geom_point(size=5) +
  geom_line(size=1) +
  #geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=affectProb-se, ymax=affectProb+se), width=0.2, color="dark gray") +
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


