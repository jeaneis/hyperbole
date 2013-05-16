d <- read.csv("../../data/mTurkExp/hyperbole/rawData/hyperbole3_long.csv", strip.white=TRUE)
d$utteredPrice <- factor(d$utteredPrice)

d.kettle <- subset(d, domain=="electric kettle")

## Ignore this for now...this just plots the average inferred price given an uttered price;
## it doesn't return a distribution 

kettle.price <- summarySE(d.kettle, measurevar="inferredPrice", groupvars=c("utteredPrice", "numberType"))
ggplot(kettle.price, aes(x=utteredPrice, y=inferredPrice, fill=numberType)) +
  geom_bar(color="black", size=0.3) +
  geom_errorbar(aes(ymin=inferredPrice-se, ymax=inferredPrice+se), size=0.3, width=0.2) +
  theme_bw()

## This plots the average estimated probability of opinion given each uttered price.
## however, we haven't combined the uttered prices yet for the sharp numbers

kettle.opinion <- summarySE(d.kettle, measurevar="probOpinion", groupvars=c("utteredPrice", "numberType"))
ggplot(kettle.opinion, aes(x=utteredPrice, y=probOpinion, fill=numberType)) +
  geom_bar(color="black", size=0.3) +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se), size=0.3, width=0.2) +
  theme_bw()


##### Creates histograms to plot distributions, so we can directly compare to model output
## plot prices 
###### IMPORTANT you can change the domain to plot data from different domains
d.domain <- subset(d, domain=="laptop")
d.domain$inferredPriceRounded <- round(d.domain$inferredPrice, 0)
utteredPrices <- c(20,21,50,51,100,101,200,201,
                   500,501,1000,1001,2000,2001,10000,10001)

# this part is just creating a list of histograms, don't worry about it for now
d.histograms <- list()
for(i in 1:length(utteredPrices)) {
  currPrice = as.character(utteredPrices[i])
  d.domain.price <- subset(d.domain, utteredPrice==currPrice)
  hist.price <- hist(d.domain.price$inferredPriceRounded, 
                     breaks=c(0,utteredPrices,4444433), plot=FALSE, include.lowest=TRUE,right=FALSE)
  hist.price.data <- data.frame(inferredPrice = hist.price$breaks, 
                                counts=c(hist.price$counts,0), utteredPrice=currPrice)
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
ggplot(d.domain.hist.trimmed, aes(x=inferredPrice, y=counts)) +
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
ggplot(d.histograms[[16]], aes(x=inferredPrice, y=counts)) +
  geom_bar(color="black", fill="#FF9999",stat="identity") +
  theme_bw()
