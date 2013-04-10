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

## plot interpretation probababilities given each utterance

d1 = read.csv("../../data/model/predict_laptop_realAffect_states14.csv")
d1 <- d1[with(d1, order(valence, meaning, utterance)), ]

d1$meaning = factor(d1$meaning)
d1$utterance = factor(d1$utterance)
d1$valence = factor(d1$valence)

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
                      ggtitle("Laptop") +          
                      theme_bw() +
                      theme(axis.text.x=element_text(angle=90, vjust=0.5, size=9))


####
# plotting interpreted meaning and interpreted valence seperately

# Moderate valence prior
dv = read.csv("valenceAnalysis_depth1_thinTail_moderateValence.csv")
dv$utterance = factor(dv$utterance)
dv$meaning = factor(dv$meaning)

dv.meaning.p <- ggplot(dv, aes(meaning, probability)) + geom_bar(stat="identity", color="black", fill="#66CC99") +
  facet_grid(. ~ utterance) +
  scale_x_discrete() +
  xlab("Meaning") +
  ylab("Probability") +                  
  opts(title="Interpreted meaning for each utterance ") +
  scale_y_continuous() +                    
  theme_bw()

dv.valence.p <- ggplot(dv, aes(meaning, postPriorRatio)) + geom_bar(stat="identity", color="black", fill="#FF9999") +
  facet_grid(. ~ utterance) +
  scale_x_discrete() +
  xlab("Meaning") +
  ylab("Valence post-prior ratio") +                  
  opts(title="Interpretated valence for each utterance ") +
  scale_y_continuous() +                       # Set tick every 
  theme_bw()

multiplot(dv.meaning.p, dv.valence.p)
