library(ggplot2)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

a <- read.csv("../../data/mTurkExp/affectPriors/raw/results_long.csv", sep=",", strip.white=TRUE)
a$probability <- a$result / 40
a.summary <- summarySE(a, measurevar="probability", groupvars=c("domain","price"))

laptop.summary <- subset(a.summary, domain=="laptop")
ggplot(laptop.summary, aes(x=price, y=probability)) +
  geom_errorbar(aes(ymin=probability-se, ymax=probability+se), width=.1) +
  geom_point() +
  geom_smooth() +
  xlab("Price") +
  ylab("Probability of affect") +
  theme_bw()+
  scale_x_continuous(breaks=seq(0,max(laptop.summary$price),100)) +
  ggtitle("Laptop affect priors")

laptop.affect.priors <- data.frame(price=laptop.summary$price, prob=laptop.summary$probability)
write.csv(laptop.affect.priors, "../../data/mTurkExp/affectPriors/laptop_affect.csv")

kettle.summary <- subset(a.summary, domain=="electric kettle")
ggplot(kettle.summary, aes(x=price, y=probability)) +
  geom_errorbar(aes(ymin=probability-se, ymax=probability+se), width=.1) +
  geom_point() +
  geom_smooth() +
  xlab("Price") +
  ylab("Probability of affect") +
  theme_bw()+
  scale_x_continuous(breaks=seq(0,max(kettle.summary$price),5)) +
  ggtitle("Electric kettle affect priors")

kettle.affect.priors <- data.frame(price=kettle.summary$price, prob=kettle.summary$probability)
write.csv(kettle.affect.priors, "../../data/mTurkExp/affectPriors/kettle_affect.csv")

coffee.summary <- subset(a.summary, domain=="coffee maker")
ggplot(coffee.summary, aes(x=price, y=probability)) +
  geom_errorbar(aes(ymin=probability-se, ymax=probability+se), width=.1) +
  geom_point() +
  geom_smooth() +
  xlab("Price") +
  ylab("Probability of affect") +
  theme_bw()+
  scale_x_continuous(breaks=seq(0,max(coffee.summary$price),10)) +
  ggtitle("Coffee maker affect priors")

coffee.affect.priors <- data.frame(price=coffee.summary$price, prob=coffee.summary$probability)
write.csv(coffee.affect.priors, "../../data/mTurkExp/affectPriors/coffee_affect.csv")

sweater.summary <- subset(a.summary, domain=="sweater")
ggplot(sweater.summary, aes(x=price, y=probability)) +
  geom_errorbar(aes(ymin=probability-se, ymax=probability+se), width=.1) +
  geom_point() +
  geom_smooth() +
  xlab("Price") +
  ylab("Probability of affect") +
  theme_bw()+
  scale_x_continuous(breaks=seq(0,max(sweater.summary$price),10)) +
  ggtitle("Sweater affect priors")

sweater.affect.priors <- data.frame(price=sweater.summary$price, prob=sweater.summary$probability)
write.csv(sweater.affect.priors, "../../data/mTurkExp/affectPriors/sweater_affect.csv")

watch.summary <- subset(a.summary, domain=="watch")
ggplot(watch.summary, aes(x=price, y=probability)) +
  geom_errorbar(aes(ymin=probability-se, ymax=probability+se), width=.1) +
  geom_point() +
  geom_smooth() +
  xlab("Price") +
  ylab("Probability of affect") +
  theme_bw()+
  scale_x_continuous(breaks=seq(0,max(watch.summary$price),100)) +
  ggtitle("Watch affect priors")

watch.affect.priors <- data.frame(price=watch.summary$price, prob=watch.summary$probability)
write.csv(watch.affect.priors, "../../data/mTurkExp/affectPriors/watch_affect.csv")

headphones.summary <- subset(a.summary, domain=="headphones")
ggplot(headphones.summary, aes(x=price, y=probability)) +
  geom_errorbar(aes(ymin=probability-se, ymax=probability+se), width=.1) +
  geom_point() +
  geom_smooth() +
  xlab("Price") +
  ylab("Probability of affect") +
  theme_bw()+
  scale_x_continuous(breaks=seq(0,max(headphones.summary$price),50)) +
  ggtitle("Headphones affect priors")

headphones.affect.priors <- data.frame(price=headphones.summary$price, prob=headphones.summary$probability)
write.csv(headphones.affect.priors, "../../data/mTurkExp/affectPriors/headphones_affect.csv")

