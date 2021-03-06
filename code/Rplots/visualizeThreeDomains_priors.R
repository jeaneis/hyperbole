p <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/prior_normalized.csv")
p$interpretation <- factor(p$interpretation)
p$interpretationRounded <- factor(p$interpretationRounded)
p.summary <- summarySE(data=p, measurevar="interpretationProb", groupvars=c("domain", "interpretationRounded"))

ggplot(p.summary, aes(x=interpretationRounded, y=interpretationProb, group=domain, color=domain)) +
  #geom_bar(stat="identity", color="black", fill="#FF9999") +
  geom_errorbar(aes(ymin=interpretationProb-se, ymax=interpretationProb+se),width=0.1, color="grey") +
  #facet_grid(domain ~ .) +
  theme_bw() +
  xlab("Price") +
  ylab("P(price)") +
  geom_point(size=5) +
  geom_line(linetype=2, size=1) +
  theme_bw() +
  theme(legend.title=element_text(size=0), legend.position=c(0.8, 0.85),
        axis.title.x=element_text(size=16), axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.y=element_text(size=16), legend.text=element_text(size=14)) +
  scale_color_manual(values=my.colors.domains)

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

d <- read.csv("../../data/mTurkExp/hyperboleThreeDomains/data_normalized.csv")
d$utteranceRounded <- factor(d$utteranceRounded)
d$utterance <- factor(d$utterance)
d$interpretationRounded <- factor(d$interpretationRounded)
d$interpretation <- factor(d$interpretation)

# compare full interpretation distribution

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


#### plot ebay scraped priors

sp <- read.csv("../../data/scrape/laptop_ebay.txt", header=FALSE)
colnames(sp) <- "price"

ggplot(sp, aes(x=price)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=50,
                 colour="black", fill="white") +
                   theme_bw() +geom_density(alpha=.2, fill="#FF6666")