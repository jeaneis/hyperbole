d.h <- read.csv("../../data/mTurkExp/hyperbole_constrained/hyperbole_constrained1_long.csv", 
                strip.white=TRUE)

current_domain = "electric kettle"
d.h.domain <- subset(d.h, domain==current_domain)
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
d.h.domain$utteredPriceLabel <- vec

d.h.d.all <- data.frame(averageScore=NA, inferred=NA, probInferred=NA,uttered=NA)[numeric(0), ]

## average first, then normalize to sum to one
for (uttered in unique(d.h.domain$utteredPriceLabel)) {
  #uttered = unique(d.h.domain$utteredPrice[1])
  d.h.d.u <- subset(d.h.domain, utteredPriceLabel==uttered)
  d.h.d.u.means <- as.data.frame(colMeans(d.h.d.u[,(10:25)]))
  d.h.d.u.means$inferred <- factor(c(20, 21, 50, 51, 100, 101, 200, 201, 500, 
                                           501, 1000, 1001, 2000, 2001, 10000, 10001))
  d.h.d.u.means$uttered <- uttered
  colnames(d.h.d.u.means)[1] <- "averageScore"
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

ggplot(d.h.d.all, aes(x=inferred, y=probInferred)) +
  geom_bar(stat="identity", color="black", fill="#FF9999") +
  facet_grid(. ~ uttered) +
  theme_bw() +
  ggtitle(paste(current_domain)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size=9))

