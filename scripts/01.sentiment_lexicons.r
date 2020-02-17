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
