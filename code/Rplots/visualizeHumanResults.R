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

### Possible domains: levels(d$domain)
selectedDomain = levels(d$domain)[1] # 6 domains: 1-6
d.domain <- subset(d, domain==selectedDomain)

### Ignore this for now...this just plots the average inferred price given an uttered price;
### it doesn't return a distribution 
# domain.price <- summarySE(d.domain, measurevar="inferredPrice", groupvars=c("roundedUtteredPrice", "numberType"))
# ggplot(domain.price, aes(x=roundedUtteredPrice, y=inferredPrice, fill=numberType)) +
#   geom_bar(color="black", size=0.3, position=position_dodge()) +
#   geom_errorbar(aes(ymin=inferredPrice-se, ymax=inferredPrice+se), size=0.3, width=0.2, position=position_dodge(0.9)) +
#   theme_bw()

## This plots the average estimated probability of opinion given each uttered price.

domain.opinion <- summarySE(d.domain, measurevar="probOpinion", 
                            groupvars=c("roundedUtteredPrice", "numberType"))

ggplot(domain.opinion, aes(x=roundedUtteredPrice, y=probOpinion, fill=numberType)) +
  geom_bar(color="black", size=0.3, position=position_dodge()) +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), size=0.3, 
                width=0.2, position=position_dodge(0.9)) +
  theme_bw() + labs(title = selectedDomain)


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
ggplot(d.domain.hist.trimmed, aes(x=inferredPrice, y=normalizedCounts)) +
  geom_bar(color="black", fill="#CCCCCC",stat="identity") +
  facet_grid(. ~ utteredPrice) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size=9))

# this plots the average probability of opinion given each uttered price
d.domain.opinion <- summarySE(d.domain, measurevar="probOpinion", groupvars=c("utteredPrice"))
ggplot(d.domain.opinion, aes(utteredPrice, probOpinion)) +
  geom_bar(color="black", fill="#FF9999",stat="identity") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), size=0.3, width=0.2) +
  theme_bw()
  
  


### ignore this; this was for debugging purposes
d.histograms[[16]]$inferredPrice = factor(d.histograms[[16]]$inferredPrice)
ggplot(d.histograms[[16]], aes(x=inferredPrice, y=normalizedCounts)) +
  geom_bar(color="black", fill="#FF9999",stat="identity") +
  theme_bw()
