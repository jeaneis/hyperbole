# read in constrained hyperbole data (in long form)
d <- read.csv("../../data/mTurkExp/hyperboleOneDomainPilot/data4_long.csv", 
                strip.white=TRUE)
nsubjects <- nrow(d) / 6

# read in price prior probs (log) in long form
d.priors <- read.csv("../../data/mTurkExp/pricePriors/pricePriors_longForm.csv")
d.laptop.priors <- subset(d.priors, domain=="laptop" & 
  (meaning==500 | meaning==501 | meaning==2000 | meaning==2001 | meaning==10000 | meaning==10001))

vec <- vector()
for (i in 1:length(d$utteredPriceRounded))
{
  val = d$utteredPriceRounded[i]
  if (d$numberType[i] == 'sharp') {
    val = val + 1
  }
  vec <- c(vec, val)
}

# set uttered price label (if the utterance wasn't round, label it as round + 1)
d$utteredPriceLabel <- vec

# create empty data frame to put inferred meanings for each uttered price label
d.inferred <- data.frame(averageScore=NA, inferred=NA, probInferred=NA,uttered=NA,probInferredSE=NA)[numeric(0), ]

## normalize to sum to one first, then average
for (uttered in unique(d$utteredPriceLabel)) {
  #uttered = unique(d.h.domain$utteredPrice[1])
  d.h.d.u <- subset(d, utteredPriceLabel==uttered)
  d.h.d.u$normalizingFactor <- rowSums(d.h.d.u[,10:15])
  for (row in c(1:nrow(d.h.d.u)))
  {
    d.h.d.u[row,10:15] <- d.h.d.u[row,10:15] / d.h.d.u$normalizingFactor[row]
  }
  
  
  d.h.d.u.means <- as.data.frame(colMeans(d.h.d.u[,(10:15)]))
  d.h.d.u.means$se <- colSds(d.h.d.u[,(10:15)]) / sqrt(nsubjects)
  d.h.d.u.means$inferred <- factor(c(500, 501 ,2000, 2001, 10000, 10001))
  d.h.d.u.means$uttered <- uttered
  colnames(d.h.d.u.means)[1] <- "probInferred"
  d.inferred <- rbind(d.inferred, d.h.d.u.means)
}

d.inferred$domain <- "laptop"
d.inferred$priorProb <- d.laptop.priors$logProb

# mark round/sharp numbers

d.inferred$numberType = ifelse((as.numeric(d.inferred$uttered) %%10 == 0), "round",  "sharp")

# label uttered as likely vs not
d.inferred$likelyhood = ifelse((as.numeric(d.inferred$uttered)==500 | as.numeric(d.inferred$uttered)==501),
                               "very likely", 
                               ifelse((as.numeric(d.inferred$uttered)==2000 | as.numeric(d.inferred$uttered)==2001),
                                      "unlikely", "very unlikely"))
d.inferred$rounded = ifelse((as.numeric(d.inferred$uttered)==500 | as.numeric(d.inferred$uttered)==501),
                               "Likely (500)", 
                               ifelse((as.numeric(d.inferred$uttered)==2000 | as.numeric(d.inferred$uttered)==2001),
                                      "Unlikely (2,000)", "Very unlikely (10,000)"))

# find all instances where the interpretation is literal
d.literal <- subset(d.inferred, inferred == uttered)
d.literal$probLiteral <- d.literal$probInferred
d.literal$probNonliteral <- 1 - d.literal$probInferred

ggplot(d.literal, aes(x=priorProb, y=probLiteral, label=uttered, color=numberType)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(ymin=probLiteral-se, ymax=probLiteral+se), width=.1) +
  theme_bw() +
  xlab("Prior probability of literal meaning (log)") +
  ylab("Probability of literal interpretation") +
  ggtitle("Humans")

ggplot(d.literal, aes(x=rounded, y=probLiteral, fill=numberType)) +
  geom_bar(position=position_dodge(), stat="identity", color="black")+
  geom_errorbar(aes(ymin=probLiteral-se, ymax=probLiteral+se),width=.2,
                    position=position_dodge(.9)) +
                      xlab("Utterred price") +
                      ylab("Probability of exact interpretation") +
                      theme_bw() +
                      theme(axis.title.x = element_text(size=18),
                            axis.text.x  = element_text(size=14),
                            axis.title.y = element_text(size=14),
                            axis.text.y = element_text(size=10))


#affect
d.opinion <- summarySE(d, measurevar="probOpinion", groupvars=c("utteredPriceRounded", "numberType"))

d.opinion$rounded = ifelse(as.numeric(d.opinion$utteredPriceRounded)==500,
                            "Likely (500)", 
                            ifelse(as.numeric(d.opinion$utteredPriceRounded)==2000,
                                   "Unlikely (2,000)", "Very unlikely (10,000)"))
ggplot(d.opinion, aes(x=rounded, y=probOpinion, fill=numberType)) +
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se),width=.2,
                position=position_dodge(.9)) +
                  ylab("Probability of expressing opinion") +
                  xlab("Uttered price") +
                  theme_bw() +
                  theme(axis.title.x = element_text(size=18),
                        axis.text.x  = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        axis.text.y = element_text(size=10))
                  
d.opinion$numberType <- factor(d.opinion$numberType)
summary(lm(data=d.opinion, probOpinion ~ numberType + priorProb))





