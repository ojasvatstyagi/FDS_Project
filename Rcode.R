rm(list=ls())
seeds <- read.csv("C:/Users/sumit/Downloads/seeds.xls", encoding = "UTF-8")

# Check for NAs
sapply(seeds, function(x) sum(is.na(x)))

# Overview of original dataset
str(seeds)
sapply(seeds, typeof)

# randomly sample 1000 rows & remove unnecessary columns
set.seed(830)
seed_sample <-seeds[sample(nrow(seeds), 1000), -c(1,5)]

# Format and transform columns
#you_sample$date <- strftime(you_sample$date, format = "%j")
seed_sample$Sentiment <- as.integer(seed_sample$Sentiment) 
seed_sample$Seed.Type <- as.factor(seed_sample$Seed.Type)
seed_sample$Website <- as.factor(seed_sample$Website)
#you_sample$Comments <- as.factor(you_sample$Comments)