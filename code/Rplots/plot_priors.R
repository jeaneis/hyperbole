library(ggplot2)
## electric kettles
prices_kettle <- read.csv("scrape/electric_kettle.txt")

prices_kettle$price = prices_kettle[,1]

mean(prices_kettle$price)
mode(prices_kettle$price)
median(prices_kettle$price)
sd(prices_kettle$price)

ggplot(kettle_trimmed, aes(x=price)) +
  geom_density() +
  theme_bw() +
  ggtitle("Electric Kettle")

## coffee maker
prices_coffee <- read.csv("scrape/coffee_maker.txt")

prices_coffee$price = prices_coffee[,1]
ggplot(prices_coffee, aes(x=price)) +
  geom_histogram(binwidth=10, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Coffee Maker")

mean(prices_coffee$price)
mode(prices_coffee$price)
median(prices_coffee$price)
sd(prices_coffee$price)

## head phones
prices_headphones <- read.csv("scrape/headphones.txt")
prices_headphones$price = prices_headphones[,1]
ggplot(prices_headphones, aes(x=price)) +
  geom_histogram(binwidth=10, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Headphones")

mean(prices_headphones$price)
mode(prices_headphones$price)
median(prices_headphones$price)
sd(prices_headphones$price)

## sweaters
prices_sweaters <- read.csv("scrape/sweater.txt")
prices_sweaters$price = prices_sweaters[,1]
ggplot(prices_sweaters, aes(x=price)) +
  geom_histogram(binwidth=10, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Sweaters")

mean(prices_sweaters$price)
mode(prices_sweaters$price)
median(prices_sweaters$price)
sd(prices_sweaters$price)

## watches
prices_watches <- read.csv("scrape/watch.txt")
prices_watches$price = prices_watches[,1]
ggplot(prices_watches, aes(x=price)) +
  geom_histogram(binwidth=50, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Watches")

mean(prices_watches$price)
mode(prices_watches$price)
median(prices_watches$price)
sd(prices_watches$price)

## laptops
prices_laptops <- read.csv("scrape/laptop.txt")
prices_laptops$price = prices_laptops[,1]
ggplot(prices_laptops, aes(x=price)) +
  geom_histogram(binwidth=50, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Laptops")

mean(prices_laptops$price)
mode(prices_laptops$price)
median(prices_laptops$price)
sd(prices_laptops$price)

## laptops-newegg
prices_laptops <- read.csv("scrape/newegg_laptop.txt")
prices_laptops$price = prices_laptops[,1]
ggplot(prices_laptops, aes(x=price)) +
  geom_histogram(binwidth=50, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Laptops")

mean(prices_laptops$price)
mode(prices_laptops$price)
median(prices_laptops$price)
sd(prices_laptops$price)


### human priors

mturk <- read.csv("mTurk_priors/data/prices1+2.csv")



## kettles
ggplot(mturk, aes(x=electric.kettle)) +
  geom_histogram(binwidth=5, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Electric Kettles")

mean(mturk$electric.kettle)

# comparison
kettle_trimmed <- subset(prices_kettle, price <= mean(prices_kettle$price) + 3 * sd(prices_kettle$price))
kettle_trimmed[,1] <- rep(c("scraped"), times=nrow(kettle_trimmed))
colnames(kettle_trimmed)[1] <- "prior_source"

kettle_mturk <- data.frame(price=mturk$electric.kettle)
kettle_mturk$prior_source <- rep(c("human"), times=nrow(kettle_mturk))

kettle_combined <- rbind(kettle_mturk, kettle_trimmed)

ggplot(kettle_combined, aes(x=price, color=prior_source)) +
  geom_density() +
  theme_bw() +
  ggtitle("Electric Kettle")

## coffee maker
ggplot(mturk, aes(x=coffee.maker)) +
  geom_histogram(binwidth=5, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Coffee Makers")

mean(mturk$coffee.maker)

# comparison
coffee_trimmed <- subset(prices_coffee, price <= mean(prices_coffee$price) + 3 * sd(prices_coffee$price))
coffee_trimmed[,1] <- rep(c("scraped"), times=nrow(coffee_trimmed))
colnames(coffee_trimmed)[1] <- "prior_source"

coffee_mturk <- data.frame(price=mturk$coffee.maker)
coffee_mturk$prior_source <- rep(c("human"), times=nrow(coffee_mturk))

coffee_combined <- rbind(coffee_mturk, coffee_trimmed)

ggplot(coffee_combined, aes(x=price, color=prior_source)) +
  geom_density() +
  theme_bw() +
  ggtitle("Coffee Makers")

## head phones
ggplot(mturk, aes(x=headphones)) +
  geom_histogram(binwidth=5, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Headphones")

mean(mturk$headphones)

# comparison
headphones_trimmed <- subset(prices_headphones, price <= mean(prices_headphones$price) + 3 * sd(prices_headphones$price))
headphones_trimmed[,1] <- rep(c("scraped"), times=nrow(headphones_trimmed))
colnames(headphones_trimmed)[1] <- "prior_source"

headphones_mturk <- data.frame(price=mturk$headphone)
headphones_mturk$prior_source <- rep(c("human"), times=nrow(headphones_mturk))

headphones_combined <- rbind(headphones_mturk, headphones_trimmed)

ggplot(headphones_combined, aes(x=price, color=prior_source)) +
  geom_density() +
  theme_bw() +
  ggtitle("Headphones")

## sweaters
ggplot(mturk, aes(x=sweater)) +
  geom_histogram(binwidth=1, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Sweater")

mean(mturk$sweater)

# comparison
sweater_trimmed <- subset(prices_sweaters, price <= mean(prices_sweaters$price) + 3 * sd(prices_sweaters$price))
sweater_trimmed[,1] <- rep(c("scraped"), times=nrow(sweater_trimmed))
colnames(sweater_trimmed)[1] <- "prior_source"

sweater_mturk <- data.frame(price=mturk$sweater)
sweater_mturk$prior_source <- rep(c("human"), times=nrow(headphones_mturk))

sweater_combined <- rbind(sweater_mturk, sweater_trimmed)

ggplot(sweater_combined, aes(x=price, color=prior_source)) +
  geom_density() +
  theme_bw() +
  ggtitle("Sweaters")


## watches
ggplot(mturk, aes(x=watch)) +
  geom_histogram(binwidth=1, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Watch")

mean(mturk$watch)

# comparison
watch_trimmed <- subset(prices_watches, price <= mean(prices_watches$price) + 3 * sd(prices_watches$price))
watch_trimmed[,1] <- rep(c("scraped"), times=nrow(watch_trimmed))
colnames(watch_trimmed)[1] <- "prior_source"

watch_mturk <- data.frame(price=mturk$watch)
watch_mturk$prior_source <- rep(c("human"), times=nrow(watch_mturk))

watch_combined <- rbind(watch_mturk, watch_trimmed)

ggplot(watch_combined, aes(x=price, color=prior_source)) +
  geom_density() +
  theme_bw() +
  ggtitle("Watches")

## laptop

ggplot(mturk, aes(x=laptop)) +
  geom_histogram(binwidth=50, colour="black", fill="white") +
  theme_bw() +
  ggtitle("Laptops")

# comparison
laptop_trimmed <- subset(prices_laptops, price <= mean(prices_laptops$price) + 3 * sd(prices_laptops$price))
laptop_trimmed[,1] <- rep(c("scraped"), times=nrow(laptop_trimmed))
colnames(laptop_trimmed)[1] <- "prior_source"

laptop_mturk <- data.frame(price=mturk$laptop)
laptop_mturk$prior_source <- rep(c("human"), times=nrow(laptop_mturk))

laptop_combined <- rbind(laptop_mturk, laptop_trimmed)

ggplot(laptop_combined, aes(x=price, color=prior_source)) +
  geom_density() +
  theme_bw() +
  ggtitle("Laptop")

