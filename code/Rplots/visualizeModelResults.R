# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## read in prior of meanings
priors <- read.csv("../../data/mTurkExp/pricePriors/fitted_normalized_pricePriors.csv")
priors <- priors[with(priors, order(domain, meaning)),]

## aggregate analysis (across domains)

## plot literal/non-literalness

d <- data.frame(utterance=NA, meaning=NA, valence=NA, 
                    probability=NA, affect_prior=NA, domain=NA, isRound=NA)
for (domain in list('kettle', 'coffee', 'headphones', 'sweater', 'laptop', 'watch')) {
  d.domain = read.csv(paste("../../data/model/predict_", domain, "_matchHumans.csv", sep=""))
  d.domain <- d.domain[with(d.domain, order(valence, meaning, utterance)), ]
  d.domain$domain = domain
  # annotate whether it's round or sharp
  d.domain$isRound <- ifelse(d.domain$utterance %% 10 == 0, 'round', 'sharp')
  d.domain$valence = factor(d.domain$valence)
  d <- rbind(d, d.domain)
}

d$isLiteral <- ifelse(d$utterance == d$meaning, 'literal',  'non-literal')
d.litAgg <- aggregate(probability ~ utterance + isLiteral + isRound + domain, data=d, FUN=sum)
d.litAgg.fig <- subset(d.litAgg, isLiteral == "non-literal")
d.litAgg.fig <- d.litAgg.fig[with(d.litAgg.fig, order(domain, utterance)), ]
# get prior probability of literal meaning
d.litAgg.fig$logPriorProb <- priors$logProb

# probability of figurative interpretation as function of roundness and prior prob of literal meaning
d.litAgg.fig$isRound = factor(d.litAgg.fig$isRound)
summary(lm(data=d.litAgg.fig, probability ~ isRound * logPriorProb))

# visualize prob figurative interpretation given prior probability

ggplot(d.litAgg.fig, aes(x=logPriorProb, y=probability, color=isRound)) +
  geom_point(position=position_jitter(width=0.5,height=0)) +
  theme_bw() +
  xlab("Prior probability of literal meaning (log)") +
  ylab("Proportion of non-literal interpretation") +
  scale_color_discrete(name="Utterance type",
                      breaks=c("round", "sharp"),
                      labels=c("Round", "Sharp")) +
                        ggtitle("Model's non-literal interpretation")
  





## read in model output
d1 = read.csv("../../data/model/predict_coffee_matchHumans.csv")
d1 <- d1[with(d1, order(valence, meaning, utterance)), ]

d1$meaning = factor(d1$meaning)
# annotate whether it's round or sharp
d1$isRound <- ifelse(d1$utterance %% 10 == 0, 'round', 'sharp')
d1$utterance = factor(d1$utterance)
d1$valence = factor(d1$valence)

## plot interpretation probababilities given each utterance
ggplot(d1, aes(x=meaning, y=probability, fill = valence)) + geom_bar(stat="identity", color="black") + 
  facet_grid(. ~ utterance) +
  scale_x_discrete() +
  scale_y_continuous() +
  xlab("") +
  #ylab("") +
  scale_fill_manual(values=c("#33CCCC", "#FF6666"),
                    guide=FALSE,
                      name="Valence",
                    breaks=c("1", "2"),
                    labels=c("No valence", "With valence")) + 
                      #ggtitle("Kettle") +          
                      theme_bw() +
                      theme(axis.text.x=element_text(angle=90, vjust=0.5, size=9))


####
# plotting interpreted meaning and interpreted valence seperately

# compute post prior affect ratio

d1.noValence = subset(d1, valence=="1")
d1.withValence = subset(d1, valence=="2")
d1$totalMeaningProb = d1.noValence$probability + d1.withValence$probability
d1.withValence = subset(d1, valence=="2")
d1.withValence$valencePosterior = d1.withValence$probability / d1.withValence$totalMeaningProb
d1.withValence$postPriorRatio = d1.withValence$valencePosterior / d1.withValence$affect_prior

# plot meaning posterior
d1.meaning.p <- ggplot(d1, aes(meaning, probability, fill=valence)) + 
  geom_bar(stat="identity", color="black") +
  facet_grid(. ~ utterance) +
  scale_x_discrete() +
  xlab("Meaning") +
  ylab("Probability") +                  
  ggtitle("Interpreted meaning for each utterance ") +
  scale_fill_manual(values=c("#33CCCC", "#FF6666"), guide=FALSE) +
  scale_y_continuous() +                    
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size=9))

# plot valence posterior
d1.valence.p <- ggplot(d1.withValence, aes(meaning, postPriorRatio)) + geom_bar(stat="identity", color="black", fill="#FF9999") +
  facet_grid(. ~ utterance) +
  scale_x_discrete() +
  xlab("Meaning") +
  ylab("Valence post-prior ratio") +                  
  ggtitle("Interpretated valence for each utterance ") +
  scale_y_continuous() +                       # Set tick every 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size=9)) +
  geom_hline(yintercept=c(1,0), linetype="dotted")

## plot degree of EXPRESSING affect given utterance
d1.withValence$expressedValence <- d1.withValence$postPriorRatio * d1.withValence$totalMeaningProb
d1.expressedValence <- aggregate(data=d1.withValence, expressedValence ~ utterance, sum)
d1.expressedValence.p <- ggplot(d1.expressedValence, aes(x=1.5, y=expressedValence)) +
  facet_grid(.~utterance) +
  geom_bar(stat="identity", color="black", fill="#CCCCCC") +
  scale_x_discrete() +
  xlab("") +
  ylab("Opinion (relative to opinion prior)") +                  
  ggtitle("Opinion expressed in each utterance ") +
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous() +                    
  theme_bw() +
  theme(axis.text.x=element_text(size=0), axis.ticks= element_blank())


multiplot(d1.meaning.p, d1.valence.p, d1.expressedValence.p)

# plot meaning posterior
d1.meaning.p <- ggplot(d1, aes(meaning, probability, fill=valence)) + 
  geom_bar(stat="identity", color="black") +
  facet_grid(. ~ utterance) +
  scale_x_discrete() +
  xlab("Inferred meaning") +
  ylab("Probability of meaning") +                  
  ggtitle("Interpreted meaning given utterance ") +
  scale_fill_manual(values=c("#33CCCC", "#FF6666"), guide=FALSE) +
  scale_y_continuous() +                    
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size=9))

## plot probability of HAVING affect given utterance
d1.havingValence <- aggregate(data=d1.withValence, probability ~ utterance, sum)
d1.opinion.p <- ggplot(d1.havingValence, aes(x=1.5, y=probability)) +
  facet_grid(.~utterance) +
  geom_bar(stat="identity", color="black", fill="#FF9999") +
  scale_x_discrete() +
  xlab("") +
  ylab("Probability of opinion") +                  
  ggtitle("Interpreted opinion given utterance") +
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous() +                    
  theme_bw() +
  theme(axis.text.x=element_text(size=0), axis.ticks= element_blank())

multiplot(d1.meaning.p, d1.opinion.p)
dev.copy(png,'testPlot.png')
dev.off()
