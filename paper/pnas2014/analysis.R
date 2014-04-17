source("summarySE.R")
source("multiplot.R")
library(ggplot2)

#########################################
# Model prdictions
#########################################
#model <- read.csv("http://stanford.edu/~justinek/hyperbole-paper/data/model-predictions.csv")
model <- read.csv("Data/Model/FullModel/predictions.csv")

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
figureS2 <- ggplot(exp1.summary, aes(x=state, y=stateProb)) +
  geom_bar(stat="identity", color="black", fill="#FF9999") +
  geom_errorbar(aes(ymin=stateProb-se, ymax=stateProb+se),width=0.2) +
  facet_grid(domain ~ utterance) +
  theme_bw() +
  ylab("Probability") +
  xlab("Interpretation") +
  ggtitle("Human") +
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=6, angle=-90),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14))

###########################
# Fit Luce choice parameter
###########################
bestFit = 0
bestAlpha = 0
for (i in seq(from=0.1, to=1, by=0.01)) {
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
  # Change column names
  colnames(model.fit.state)[7] <- "model"
  compare <- join(exp1.summary, model.fit.state, by=c("utterance", "state", "domain", "interpretationKind"))
  colnames(compare)[10] <- "human"
  
  r <- with(compare, cor(human, model))
  if (r > bestFit) {
    bestFit <- r
    bestAlpha <- i
  }
}

##########################
# Compute model predictions with best parameter fit
##########################
model$raisedProb <- model$probability^bestAlpha
normalizingFactors <- aggregate(data=model, raisedProb ~ domain + utterance, FUN=sum)
colnames(normalizingFactors)[3] <- "normalizing"
model <- join(model, normalizingFactors, by=c("domain", "utterance"))
model$modelProb <- model$raisedProb / model$normalizing

# Marginalize over affect
model.state <- aggregate(data=model, modelProb ~ 
                           domain + utterance + state + utteranceRounded + stateRounded + interpretationKind, FUN=sum)

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
  scale_color_manual(values=my.colors, name="Interpretation Kind", 
                     breaks=c("exact", "fuzzy", "hyperbolic", "other"),
                     labels=c("Exact", "Fuzzy", "Hyperbolic", "Other"))+
  scale_shape_discrete(name="Item", breaks=c("electric kettle", "laptop", "watch"),
                       labels=c("Electric Kettle", "Laptop", "Watch"))

with(compare, cor.test(modelProb, stateProb))

#################
# Fig. 2(B)
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

model.noAffectGoal <- makeModel("http://stanford.edu/~justinek/hyperbole-paper/data/model-predictions-noAffectGoal")
model.noImpreciseGoal <- makeModel("http://stanford.edu/~justinek/hyperbole-paper/data/model-predictions-noImpreciseGoal")

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
# my.colors.goals<- c("#d9d9d9", "#dadaeb", "#bcbddc", "#9edae5", "#17becf")
#### colors for presentation
my.colors.goals <- c("#d9d9d9", "#dadaeb", "#bcbddc", "#cc6699", "#ff9896")
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
# Figure 3A
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

model.noPriors <- makeModel("http://stanford.edu/~justinek/hyperbole-paper/data/model-predictions-noPrior.csv")
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



######
# Hyperbole
######

######
# Halo
######