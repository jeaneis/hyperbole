# read in constrained hyperbole data (in long form)
d.h <- read.csv("../../data/mTurkExp/hyperbole_constrained/hyperbole_constrained1_long.csv", 
                strip.white=TRUE)

# read in price prior probs (log) in long form
d.priors <- read.csv("../../data/mTurkExp/pricePriors/pricePriors_longForm.csv")
  
d.h.allDomains <- data.frame(averageScore=NA, inferred=NA, uttered=NA, probInferred=NA, domain=NA, priorProb=NA)[numeric(0), ]

for (current_domain in levels(d.h$domain)) {
  # select domain
  d.h.domain <- subset(d.h, domain==current_domain)
  
  # set rounded uttered price 
  d.h.domain$roundedUtteredPrice <- round(d.h.domain$utteredPrice / 10.0) * 10
  
  vec <- vector()
  for (i in 1:length(d.h.domain$roundedUtteredPrice))
  {
    val = d.h.domain$roundedUtteredPrice[i]
    if (d.h.domain$numberType[i] == 'sharp') {
      val = val + 1
    }
    vec <- c(vec, val)
  }
  
  # set uttered price label (if the utterance wasn't round, label it as round + 1)
  d.h.domain$utteredPriceLabel <- vec
  
  # create empty data frame to put inferred meanings for each uttered price label
  d.h.d.all <- data.frame(averageScore=NA, inferred=NA, probInferred=NA,uttered=NA)[numeric(0), ]
  
  ## average first, then normalize to sum to one
  for (uttered in unique(d.h.domain$utteredPriceLabel)) {
    # select subset with particular uttered price label
    d.h.d.u <- subset(d.h.domain, utteredPriceLabel==uttered)
    # calculate the mean ratings for each uttered price
    d.h.d.u.means <- as.data.frame(colMeans(d.h.d.u[,(10:25)]))
    # set the labels of the inferred meanings
    d.h.d.u.means$inferred <- factor(c(20, 21, 50, 51, 100, 101, 200, 201, 500, 
                                       501, 1000, 1001, 2000, 2001, 10000, 10001))
    # set the labls of the uttered prices
    d.h.d.u.means$uttered <- uttered
    # change the column name
    colnames(d.h.d.u.means)[1] <- "averageScore"
    # normalize across ratings for each inferred meaning to sum to 1
    normalizingFactor = sum(d.h.d.u.means$averageScore)
    d.h.d.u.means$probInferred <- d.h.d.u.means$averageScore / normalizingFactor
    d.h.d.all <- rbind(d.h.d.all, d.h.d.u.means)
  }
  
  ## normalize to sum to one first, then average
  for (uttered in unique(d.h.domain$utteredPriceLabel)) {
    #uttered = unique(d.h.domain$utteredPrice[1])
    d.h.d.u <- subset(d.h.domain, utteredPriceLabel==uttered)
    d.h.d.u$normalizingFactor <- rowSums(d.h.d.u[,10:25])
    for (row in c(1:nrow(d.h.d.u)))
    {
      d.h.d.u[row,10:25] <- d.h.d.u[row,10:25] / d.h.d.u$normalizingFactor[row]
    }
    
    d.h.d.u.means <- as.data.frame(colMeans(d.h.d.u[,(10:25)]))
    d.h.d.u.means$inferred <- factor(c(20, 21, 50, 51, 100, 101, 200, 201, 500, 
                                       501, 1000, 1001, 2000, 2001, 10000, 10001))
    d.h.d.u.means$uttered <- uttered
    colnames(d.h.d.u.means)[1] <- "probInferred"
    d.h.d.all <- rbind(d.h.d.all, d.h.d.u.means)
  }
  
  d.h.d.all$domain <- current_domain
  d.priors.domain <- subset(d.priors, domain==current_domain)
  d.h.d.all$priorProb <- d.priors.domain$logProb
  d.h.allDomains <- rbind(d.h.allDomains, d.h.d.all)
  
  p <- ggplot(d.h.d.all, aes(x=inferred, y=probInferred)) +
    geom_bar(stat="identity", color="black", fill="#FF9999") +
    facet_grid(. ~ uttered) +
    theme_bw() +
    ggtitle(paste(current_domain)) +
    theme(axis.text.x=element_text(angle=90, vjust=0.5, size=9)) +
    ylab("Probability") +
    xlab("Inferred Meaning")
}


#### aggregate analysis of literalness
# mark round/sharp numbers

d.h.allDomains$numberType = ifelse((as.numeric(d.h.allDomains$uttered) %%10 == 0), "round",  "sharp")

# find all instances where the interpretation is literal
d.h.allDomains.literal <- subset(d.h.allDomains, inferred == uttered)
d.h.allDomains.literal$probLiteral <- d.h.allDomains.literal$probInferred
d.h.allDomains.literal$probNonliteral <- 1 - d.h.allDomains.literal$probInferred

ggplot(d.h.allDomains.literal, aes(x=priorProb, y=probNonliteral, color=numberType)) +
  geom_point(position=position_jitter(width=0,height=0), size=2) +
  theme_bw() +
  xlab("Prior probability of literal meaning (log)") +
  ylab("Probability of non-literal interpretation") +
  scale_color_discrete(name="Utterance type",
                       breaks=c("round", "sharp"),
                       labels=c("Round", "Sharp")) +
                         ggtitle("Humans' non-literal interpretation")

d.h.allDomains.literal$numberType = factor(d.h.allDomains.literal$numberType)
d.h.allDomains.literal <- d.h.allDomains.literal[with(d.h.allDomains.literal, 
                                                      order(domain, as.numeric(uttered))), ]

summary(lm(data=d.h.allDomains.literal, log(probNonliteral) ~ numberType))


### compare with model
comp.figurativeness <- data.frame(utterance=d.litAgg$utteredPriceLabel, 
                                  model=d.litAgg.fig$probability, 
                                  human=d.h.allDomains.literal$probNonliteral,
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

#### Opinion analysis
d.h.domain.opinionMeans <- summarySE(data=d.h.domain, measurevar="probOpinion", groupvars=c("utteredPriceLabel"))
ggplot(d.h.domain.opinionMeans, aes(x=1.5, y=probOpinion)) +
  geom_bar(stat="identity", color="black", fill="#FF9999") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), width=0.2) +
  facet_grid(.~utteredPriceLabel) +
  theme_bw() +
  xlab("")
