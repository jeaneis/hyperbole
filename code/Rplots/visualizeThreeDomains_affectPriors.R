a <- read.table("../../data/mTurkExp/hyperboleThreeDomains/affect_long.csv", 
                strip.white=TRUE, header=TRUE, sep=",")
a$utteredPriceRounded <- factor(a$utteredPriceRounded)
a.summary <- summarySE(a, measurevar="probOpinion", groupvars=c("domain", "utteredPriceRounded"))


my.colors.domains <- c("#ff9896", "#17becf", "#e7ba52")
ggplot(a.summary, aes(x=utteredPriceRounded, y=probOpinion, color=domain, group=domain)) +
  geom_point(size=5) +
  geom_line(linetype=2, size=1) +
  #geom_bar(stat="identity", color="black", fill="gray") +
  geom_errorbar(aes(ymin=probOpinion-se, ymax=probOpinion+se),width=0.1, color="grey") +
  #facet_grid(domain ~ .) +
  theme_bw() +
  xlab("Price") +
  ylab("P(affect | price)") +
  theme(legend.title=element_text(size=0), legend.position=c(0.8, 0.2),
        axis.title.x=element_text(size=16), axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=16), axis.text.y=element_text(size=14),
        strip.text.y=element_text(size=16), legend.text=element_text(size=14)) +
  scale_color_manual(values=my.colors.domains)

# record the affect priors

write.csv(a.summary, "../../data/mTurkExp/affectPriors/affect-10182013.csv")