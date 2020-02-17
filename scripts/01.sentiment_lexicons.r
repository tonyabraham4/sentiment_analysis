# Sentiment lexicons
# There are several different sentiment lexicons available for sentiment analysis. 
# You will explore three in this course that are available in the tidytext package:
#   
# afinn from Finn Årup Nielsen,
# bing from Bing Liu and collaborators, and
# nrc from Saif Mohammad and Peter Turney.

# Load dplyr and tidytext
library(dplyr)
library(tidytext)

# Choose the bing lexicon
get_sentiments("bing")

# Choose the nrc lexicon
get_sentiments("nrc") %>%
  count(sentiment) # Count words by sentiment

# geocoded_tweets has been pre-defined
geocoded_tweets

# Access bing lexicon: bing
bing <- get_sentiments("bing")

# Use data frame with text data
geocoded_tweets %>%
  # With inner join, implement sentiment analysis using `bing`
  inner_join(geocoded_tweets, bing, by = "word")

# tweets_nrc has been pre-defined
tweets_nrc

tweets_nrc %>%
  # Filter to only choose the words associated with sadness
  filter(sentiment == "sadness") %>%
  # Group by word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarise(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# What are the most common joy words?
# You can use the same approach from the last exercise to find the most common words associated with joy in these tweets. 
# Use the same pattern of dplyr verbs to find a new result.

# tweets_nrc has been pre-defined
tweets_nrc

joy_words <- tweets_nrc %>%
  # Filter to choose only words associated with joy
  filter(sentiment == "joy") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarise(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))    

joy_words %>%
  top_n(20) %>%
  mutate(word = reorder(word, freq)) %>%
  # Use aes() to put words on the x-axis and frequency on the y-axis
  ggplot(aes(x=word, y= freq)) +
  # Make a bar chart with geom_col() #Use geom_col() to make a bar chart. (If you are familiar with geom_bar(stat = "identity"), geom_col() does the same thing.)
  geom_col() +
  coord_flip()


# Do people in different states use different words?
# So far you have looked at the United States as a whole, but you can use this dataset to examine differences in word use by state.
# In this exercise, you will examine two states and compare their use of joy words. Do they use the same words associated with joy? 
# Do they use these words at the same rate?

# tweets_nrc has been pre-defined
tweets_nrc

tweets_nrc %>%
  # Find only the words for the state of Utah and associated with joy
  filter(state == "utah",
         sentiment == "joy") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

tweets_nrc %>%
  # Find only the words for the state of Louisiana and associated with joy
  filter(state == "louisiana",
         sentiment == "joy") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq)) 


# Which states have the most positive Twitter users?
# For the last exercise in this chapter, you will determine how the overall sentiment of Twitter sentiment varies from state to state.
# You will use a dataset called tweets_bing, which is the output of an inner join created just the same way that you did earlier. 
# Check out what tweets_bing looks like in the console.

# You can use group_by() and summarize() to find which states had the highest frequency of positive and negative words, 
# then pipe to ggplot2 (after some tidyr manipulation) to make a clear, interesting visualization.

# tweets_bing has been pre-defined
tweets_bing

tweets_bing %>% 
  # Group by two columns: state and sentiment
  group_by(state, sentiment) %>%
  # Use summarize to calculate the mean frequency for these groups
  summarize(freq = mean(freq)) %>%
  spread(sentiment, freq) %>%
  ungroup() %>%
  # Calculate the ratio of positive to negative words
  mutate(ratio = positive / negative,
         state = reorder(state, ratio)) %>%
  # Use aes() to put state on the x-axis and ratio on the y-axis
  ggplot(aes(state, ratio)) +
  # Make a plot with points using geom_point()
  geom_point() +
  coord_flip()
