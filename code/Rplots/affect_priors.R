mturk <- read.csv("../../data/mTurkExp/pricePriors/prices1+2.csv")

# make vector of prices by fixed intervals
rounded.kettle <- round(seq(from=min(mturk$electric.kettle), to=max(mturk$electric.kettle), length.out=20), 2)
rounded.coffee <- round(seq(from=min(mturk$coffee.maker), to=max(mturk$coffee.maker), length.out=20), 2)
rounded.headphones <- round(seq(from=min(mturk$headphones), to=max(mturk$headphones), length.out=20), 2)
rounded.sweater <- round(seq(from=min(mturk$sweater), to=max(mturk$sweater), length.out=20), 2)
rounded.watch <- round(seq(from=min(mturk$watch), to=max(mturk$watch), length.out=20), 2)
rounded.laptop <- round(seq(from=min(mturk$laptop), to=max(mturk$laptop), length.out=20), 2)

prices.rounded <- data.frame(kettle=rounded.kettle, coffee=rounded.coffee, 
                             headphones=rounded.headphones, sweater=rounded.sweater,watch=rounded.watch,
                             laptop=rounded.laptop)

write.csv(prices.rounded, "../../data/mTurkExp/pricePriors/prices_rounded.csv")