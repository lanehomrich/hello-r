# Lane Homrich
# lhomrich3@gatech.edu
# GTID: 903195576

# Project Checkpoint 4
# R in action

# Data Transformations and Exploratory Data Analysis

install.packages("tidyverse") # a set of data science tools including dplyr, tidyr and stringr
install.packages("skimr") # a package to facilitate data summaries
install.packages("Hmisc") # a package for data analysis
install.packages("rjson")
install.packages("topicmodels")
install.packages("corpus")
install.packages("tidytext")

library("rjson")
library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)

# Get all businesses
businesses <- stream_in(file("business.json"))
head(businesses, 10)
# Filter by restaurants
restaurants <- businesses %>% filter(str_detect(categories, "Restaurant"))
head(restaurants, 10)
# Remove some unnecessary columns
restaurants <- restaurants %>% select(-starts_with("attribute"))
head(restaurants, 10)
summary(restaurants)

# Make tables for data with average ratings, total businesses, and total reviews per state
ratingsTable <- restaurants %>% group_by(state) %>% summarise(averageRating = mean(stars))
businessesTable <- restaurants %>% group_by(state) %>% summarise(totalBusinesses = n())
reviewsTable <- restaurants %>% group_by(state) %>% summarise(totalReviews = sum(review_count))

# Make graphs showing that data
qplot(x=averageRating, y=state, data=ratingsTable)
qplot(x=totalBusinesses, y=state, data=businessesTable)
qplot(x=totalReviews, y=state, data=reviewsTable)

# Read in abridged review file
# I plan on using more of the dataset for final project presentation
reviews <- stream_in(file("review_abridged.json"))

# Load the packages required to create an LDA model
library("rjson")
library(topicmodels)
library("corpus")
library(tm)
library(tidytext)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

head(reviews)

# Makes a corpus before the conversion to pass into the DocumentTermMatrix method
corpus <- Corpus(VectorSource(reviews$text))
# Document Term Matrix for the reviews text
dtm <- DocumentTermMatrix(corpus)

# Tidy up the Document Term Matrix
tidy <- tidy(dtm)
help(tidy)

# Remove all stop words (like the, I, etc.)
reviewsDTM <- tidy %>% anti_join(stop_words, by = c("term" = "word"))

# Set the data up into a Document Term Matrix, required by the LDA function
dtmNoStopWords <- reviewsDTM %>% cast_dtm(document, term, count)

# Pick a random seed and 4 topics (seed and number of topics chosen randomly)
ldaNoStopWords <- LDA(dtmNoStopWords, k=4, control = list(seed = 2222))
ldaNoStopWords

# Look at the beta distribution of terms related to the other topics
topicDistributionNoStopWords <- tidy(ldaNoStopWords, matrix="beta")
topicDistributionNoStopWords

# Plot the 20 most commonly used words in each of the 4 topics
top_words <- topicDistributionNoStopWords %>%
  group_by(topic) %>% top_n(20, beta) %>% ungroup()

# Still trying to figure out how to make darkorchid4
top_words %>%
  mutate(word=reorder_within(term, beta, topic)) %>%
  ggplot(aes(word, beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales="free") +
  scale_x_reordered() +
  coord_flip()
