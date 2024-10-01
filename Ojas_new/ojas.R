rm(list=ls())
seeds <- read.csv("C:/Users/sumit/Downloads/seeds.csv", encoding = "UTF-8")

# Check for NAs
sapply(seeds, function(x) sum(is.na(x)))

# Overview of original dataset
str(seeds)
sapply(seeds, typeof)

# randomly sample 1000 rows & remove unnecessary columns
set.seed(830)
seed_sample <-seeds[sample(nrow(seeds), 1000), -c(1,2,6)]

# Format and transform columns
seed_sample$Date <- strftime(seed_sample$Date, format = "%j")
seed_sample$Date <- as.integer(seed_sample$Date)
seed_sample$Sentiment <- as.integer(seed_sample$Sentiment) 
seed_sample$Seed.Type <- as.factor(seed_sample$Seed.Type)
seed_sample$Website <- as.factor(seed_sample$Website)
seed_sample$Comment <- as.factor(seed_sample$Comment)

# Double-check format
sapply(seed_sample, typeof)


# * default parameters
processed <- textProcessor(seed_sample$Comment, metadata = seed_sample,
                           lowercase = TRUE, #*
                           removestopwords = TRUE, #*
                           removenumbers = TRUE, #*
                           removepunctuation = TRUE, #*
                           stem = TRUE, #*
                           wordLengths = c(3,Inf), #*
                           sparselevel = 1, #*
                           language = "en", #*
                           verbose = TRUE, #*
                           onlycharacter = TRUE, # not def
                           striphtml = FALSE, #*
                           customstopwords = NULL, #*
                           v1 = FALSE) #*


# filter out terms that don’t appear in more than 10 documents,
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh=10)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# Check levels
levels(meta$publisher)
levels(meta$stock)



set.seed(831)
system.time({
  First_STM <- stm(docs, vocab, 15, 
                   prevalence =~ factor(Sentiment),  # or just ~1 if that doesn't work
                   data = meta, 
                   seed = 15, 
                   max.em.its = 75)
})
# Plot first Topic Model
plot(First_STM)




set.seed(832)
system.time({
  Second_STM <- stm(documents = out$documents, vocab = out$vocab,
                    K = 18, prevalence =~ factor(Sentiment),
                    max.em.its = 75, data = out$meta,
                    init.type = "Spectral", verbose = FALSE
  )
})

# Plot second Topic Model
plot(Second_STM)



# Find k: Approach 1 
set.seed(833)
system.time({
  findingk <- searchK(out$documents, out$vocab, K = c(10:30),
                      prevalence =~ factor(Sentiment), data = meta, verbose=FALSE
  )
})

# Plot
plot(findingk)




# Find k: Approach 2
set.seed(834)
system.time({
  findingk_ver2 <- searchK(documents = out$documents, 
                           vocab = out$vocab,
                           K = c(10,20,30,40,50,60, 70), #specify K to try
                           N = 500, # matches 10% default
                           proportion = 0.5, # default
                           heldout.seed = 1234, # optional
                           M = 10, # default
                           cores = 1, # default=1
                           prevalence =~  factor(Sentiment),
                           max.em.its = 75, #was 75
                           data = meta,
                           init.type = "Spectral",
                           verbose=TRUE
  )
})

# Plot
plot(findingk_ver2)

#update.packages()

# Find k: Approach 3
set.seed(835)
system.time({
  findingk_ver3.lee_mimno <- stm(documents = out$documents, 
                                 vocab = out$vocab,
                                 K = 0, # K=0 instructs STM to run Lee-Mimno
                                 seed = 1234, # randomness now, seed matters
                                 prevalence =~ factor(Sentiment),
                                 max.em.its = 75,
                                 data = meta,
                                 init.type = "Spectral",
                                 verbose=TRUE
  )
})


# Plot
plot(findingk_ver3.lee_mimno)


# Run final topic model at 20 topics and see how long it takes
set.seed(836)
system.time({
  Third_STM <- stm(documents = out$documents, vocab = out$vocab,
                   K = 10, prevalence =~ factor(Sentiment),
                   max.em.its = 75, data = out$meta,
                   init.type = "Spectral", verbose = FALSE
  )
})

#Plot
plot(Third_STM)

#Step2
# Top Words
labelTopics(Third_STM)

# We can find the top documents associated with a topic with the findThoughts function:
# top 2 paragraps for Topic 1 to 10

findThoughts(Third_STM, texts = meta$Comments,n = 2, topics = 1:10)


# We can look at multiple, or all, topics this way as well. 
# For this we’ll just look at the shorttext.
# top 3 paragraps for Topic #1 to 15

findThoughts(Third_STM, texts = meta$Comments,n = 3, topics = 1:10)



# Graphical display of topic correlations

topic_correlation<-topicCorr(Third_STM)
plot(topic_correlation)


# Graphical display of convergence

plot(Third_STM$convergence$bound, type = "l",
     ylab = "Approximate Objective",
     main = "Convergence")

# Wordcloud:topic 17 with word distribution

set.seed(837)
cloud(Third_STM, topic=10, scale=c(10,2))


set.seed(831)
plot(Third_STM, 
     type="perspectives", 
     topics=c(10,9), 
     plabels = c("Topic 10","Topic 9"))





# Topic proportions

plot(Third_STM, type = "hist", topics = sample(1:10, size = 9))
plot(Third_STM, type="hist")




# The topicQuality() function plots these values 
# and labels each with its topic number:

topicQuality(model=Third_STM, documents=docs)
