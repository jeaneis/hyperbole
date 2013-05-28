source("~/Dropbox/toolbox/R/summarySE.R")
library(ggplot2)

d <- read.csv("../../data/mTurkExp/hyperbole/rawData/hyperbole3.2_long.csv", strip.white=TRUE)

### Group sharp numbers of the same price point and label them
d$roundedUtteredPrice <- round(d$utteredPrice / 10.0) * 10

vec <- vector()
for (i in 1:length(d$roundedUtteredPrice))
{
  val = d$roundedUtteredPrice[i]
  if (d$numberType[i] == 'sharp') {
    val = val + 1
  }
  vec <- c(vec, val)
}

d$utteredPriceLabel <- factor(vec)
d$roundedUtteredPrice <- factor(d$roundedUtteredPrice)

### check how many responses were given for each domain X round/sharp X roundedUtteredPrice
numSubjectsCount <- summarySE(d, measurevar="inferredPrice", 
                              groupvars=c("domain", "numberType", "roundedUtteredPrice"))

plot(d$roundedUtteredPrice, d$numberType)

### Bucket inferred price into one of the 8 price points
vec <- vector()
interpretedPrices = as.numeric(levels(unique(d$roundedUtteredPrice)))
for (i in d$inferredPrice)
{
  delta <- abs(i - interpretedPrices)
  vec <- c(vec, interpretedPrices[which.min(delta)])
}
d$bucketedInferredPrice <- vec

### Label inferred price into sharp/round
vec <- vector()
for (i in 1:length(d$roundedUtteredPrice))
{
  val = d$bucketedInferredPrice[i]
  if (d$inferredPrice[i] != val)
  {
    val = val + 1
  }
  vec <- c(vec, val)
}
d$inferredPriceLabel <- vec


#### aggregate analysis of literal/figurative interpretation
# code interpretation as literal/non-literal based on
# whether utteredPrice and inferredPrice are
# exactly the same or not

d$isLiteral <- ifelse(d$utteredPrice == round(d$inferredPrice, 0), 1,  0)
d.litAgg <- aggregate(isLiteral ~ utteredPriceLabel + numberType + domain, data=d, FUN=mean)
d.litAgg$isFigurative <- 1-d.litAgg$isLiteral
d.litAgg <- d.litAgg[with(d.litAgg, order(domain, as.numeric(utteredPriceLabel))), ]
# get prior probability of literal meaning
d.litAgg$logPriorProb <- priors$logProb

ggplot(d.litAgg, aes(x=logPriorProb, y=isFigurative, color=domain, shape=numberType)) +
  geom_point(position=position_jitter(width=0.5,height=0), size=4) +
  theme_bw() +
  xlab("Prior probability of literal meaning (log)") +
  ylab("Proportion of non-literal interpretation") +
  scale_color_discrete(name="Utterance type",
                       breaks=c("round", "sharp"),
                       labels=c("Round", "Sharp")) +
                         ggtitle("Humans' non-literal interpretation")

summary(lm(data=d.litAgg, isFigurative ~ logPriorProb * numberType))

# analyze distance between interpretaiton and uttered price
d$interpDistance <- abs(d$utteredPrice - d$inferredPrice)
d.distAgg <- aggregate(interpDistance ~ utteredPriceLabel + numberType + domain, data=d, FUN=mean)
d.distAgg <- d.distAgg[with(d.distAgg, order(domain, as.numeric(utteredPriceLabel))), ]
# get prior probability of literal meaning
d.distAgg$logPriorProb <- priors$logProb

ggplot(d.distAgg, aes(x=logPriorProb, y=interpDistance, color=numberType)) +
  geom_point(position=position_jitter(width=0.5,height=0)) +
  theme_bw() +
  xlab("Prior probability of literal meaning (log)") +
  ylab("Proportion of non-literal interpretation") +
  scale_color_discrete(name="Utterance type",
                       breaks=c("round", "sharp"),
                       labels=c("Round", "Sharp")) +
                         ggtitle("Human interpretation distance")

summary(lm(data=d.distAgg, interpDistance ~ logPriorProb * numberType))
summary(lm(data=d.distAgg, interpDistance ~ numberType))


### compare with model
comp.figurativeness <- data.frame(utterance=d.litAgg$utteredPriceLabel, 
                                  model=d.litAgg.fig$probability, 
                                  human=d.litAgg$isFigurative,
                                  numberType=d.litAgg$numberType)
ggplot(comp.figurativeness, aes(x=model, y=human, color=numberType)) +
  geom_point() +
  theme_bw() +
  geom_abline(yintercept=0,slope=1, linetype="dashed") +
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  ggtitle("Human and model comparision of non-literalness")

with(comp.figurativeness, cor.test(model, human))
summary(lm(data=comp.figurativeness, human ~ model))

### Possible domains: levels(d$domain)
for (j in 1:6) {
  j = 1
  selectedDomain = levels(d$domain)[j] # 6 domains: 1-6
  d.domain <- subset(d, domain==selectedDomain)
  
  ## affect analysis
  # get mean probOpinion given an utteredPriceLabel and inferredPriceLabel
  d.affectAgg <- summarySE(data=d.domain, measurevar="probOpinion", groupvars=c("inferredPriceLabel", "utteredPriceLabel"))
  # reorder
  d.affectAgg <- d.affectAgg[with(d.affectAgg, order(as.numeric(utteredPriceLabel), inferredPriceLabel)), ]
  
  # get total number of responses for each utteredPriceLabel
  d.utteredAgg <- aggregate(data=d.affectAgg, N ~ utteredPriceLabel, FUN=sum)
  # make utteredAgg into a dictionary
  d.utteredTotal <- d.utteredAgg$N
  names(d.utteredTotal) <- d.utteredAgg$utteredPriceLabel
  
  # read affect prior
  affect.prior <- read.csv("../../data/mTurkExp/affectPriors/fitted_affectPriors_coffee.csv")
  # turn affect prior into a dictionary
  affect.prior.dict <- affect.prior$prob
  names(affect.prior.dict) <- affect.prior$meaning
  
  for (i in 1:nrow(d.affectAgg)) {
    u = as.character(d.affectAgg$utteredPriceLabel[i])
    n = d.utteredTotal[[u]]
    d.affectAgg$totalUttered[i] = n
    
    m = as.character(d.affectAgg$inferredPriceLabel[i])
    a = affect.prior.dict[[m]]
    d.affectAgg$affectPrior[i] = a
  }
  d.affectAgg$probInferredPrice <- d.affectAgg$N / d.affectAgg$totalUttered
  d.affectAgg$postPriorRatio <- d.affectAgg$probOpinion / d.affectAgg$affectPrior
  d.affectAgg$weightedExpressedAffect <- d.affectAgg$probInferredPrice * d.affectAgg$postPriorRatio
  
  d.affect.byUtterance <- aggregate(data=d.affectAgg, weightedExpressedAffect ~ utteredPriceLabel,
                                    FUN = sum)
  
  ggplot(d.affect.byUtterance, aes(x=1.5, y=weightedExpressedAffect)) +
    facet_grid(.~utteredPriceLabel) +
    geom_bar(color="black", fill="#FF9999",stat="identity") +
    theme_bw() +
    scale_x_discrete() +
    xlab("") +
    theme(axis.text.x=element_text(size=0), axis.ticks= element_blank())
  
  ### Ignore this for now...this just plots the average inferred price given an uttered price;
  ### it doesn't return a distribution 
  # domain.price <- summarySE(d.domain, measurevar="inferredPrice", groupvars=c("roundedUtteredPrice", "numberType"))
  # ggplot(domain.price, aes(x=roundedUtteredPrice, y=inferredPrice, fill=numberType)) +
  #   geom_bar(color="black", size=0.3, position=position_dodge()) +
  #   geom_errorbar(aes(ymin=inferredPrice-se, ymax=inferredPrice+se), size=0.3, width=0.2, position=position_dodge(0.9)) +
  #   theme_bw()
  
  ### This plots the average estimated probability of opinion given each uttered price.
  #domain.opinion <- summarySE(d.domain, measurevar="probOpinion", 
  #                            groupvars=c("roundedUtteredPrice", "numberType"))
  
  #ggplot(domain.opinion, aes(x=roundedUtteredPrice, y=probOpinion, fill=numberType)) +
  #  geom_bar(color="black", size=0.3, position=position_dodge(), stat="identity") +
  #  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), size=0.3, 
  #                width=0.2, position=position_dodge(0.9)) +
  #  theme_bw() + labs(title = selectedDomain)
  
  ##### Creates histograms to plot distributions, so we can directly compare to model output
  ## plot prices 
  ###### IMPORTANT you can change the domain to plot data from different domains
  
  utteredPrices <- levels(d$utteredPriceLabel)
  
  # this part is just creating a list of histograms
  d.histograms <- list()
  for(i in 1:length(utteredPrices)) {
    currPrice = utteredPrices[i]
    d.domain.price <- subset(d.domain, utteredPriceLabel==currPrice)
    hist.price <- hist(d.domain.price$inferredPriceLabel, 
                       breaks=c(as.numeric(utteredPrices), max(d$inferredPrice)), plot=FALSE, include.lowest=TRUE,right=FALSE)
    hist.price.data <- data.frame(inferredPrice = hist.price$breaks, 
                                  counts=c(hist.price$counts, 0), utteredPrice=currPrice)
    numSubjects <- dim(d.domain.price)[1] # number of subjects for this uttered price label
    hist.price.data$normalizedCounts = hist.price.data$counts / numSubjects
    d.histograms[[i]] <- hist.price.data
  }
  
  d.domain.hist <- d.histograms[[1]]
  for(j in 2:length(utteredPrices)) {
    d.domain.hist <- rbind(d.domain.hist, d.histograms[[j]])
  }
  
  d.domain.hist$inferredPrice <- hist.price.data$inferredPrice
  d.domain.hist.trimmed <- subset(d.domain.hist, inferredPrice <= 10001)
  d.domain.hist.trimmed$inferredPrice <- factor(d.domain.hist.trimmed$inferredPrice)
  
  # this plots the distribution of inferred prices given each uttered price
  q<-ggplot(d.domain.hist.trimmed, aes(x=inferredPrice, y=normalizedCounts)) +
    geom_bar(color="black", fill="#CCCCCC",stat="identity") +
    facet_grid(. ~ utteredPrice) +
    theme_bw() + 
    ggtitle("Interpreted meaning given utterance ") +
    ylab("Probability of meaning") +
    xlab("Inferred meaning") +
    #ggtitle(selectedDomain) +
    theme(axis.text.x=element_text(angle=90, vjust=0.5, size=9))
  
  # this plots the average probability of opinion given each uttered price
  d.domain.opinion <- summarySE(d.domain, measurevar="probOpinion", groupvars=c("utteredPriceLabel"))
  o<-ggplot(d.domain.opinion, aes(x=1.5, probOpinion)) +
    facet_grid(.~utteredPriceLabel) +
    geom_bar(color="black", fill="#FF9999",stat="identity") +
    geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), width=0.2) +
    theme_bw() + 
    ggtitle("Interpreted opinion given utterance") +
    theme(axis.text.x=element_text(size=0), axis.ticks= element_blank()) +
    xlab("") +
    scale_x_discrete() +
    ylab("Probability of opinion")
  multiplot(q, o)
  
  f <- paste("../../data/Rplots/mTurkHyperbole/", "interp_", selectedDomain,".png", sep="")
  ggsave(q, width=30,height=7,filename = f,dpi=72) 
}


  


### ignore this; this was for debugging purposes
d.histograms[[16]]$inferredPrice = factor(d.histograms[[16]]$inferredPrice)
ggplot(d.histograms[[16]], aes(x=inferredPrice, y=normalizedCounts)) +
  geom_bar(color="black", fill="#FF9999",stat="identity") +
  theme_bw()
