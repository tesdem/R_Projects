
#Text Mining in R
#Twitter: Trump Tweets
#Reference book: 
#Introduction to Data Science: Data Analysis and Prediction Algorithms with R


#set working directory
path <- "C:/Users/Tesfaye D/Desktop/Projects/R/Text Mining"
setwd(path)

library(tidyverse)
library(lubridate)
library(scales)

#get the data from JSON API using a script
url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, 
                                      orders = "a b! d! H!:M!:S! z!* Y!",
                                      tz="EST")) 

#the output data is saved in 'dslabs' package - for convenience
install.packages("dslabs")
library(dslabs)
data("trump_tweets")
head(trump_tweets) # few top observations
names(trump_tweets) #dataset variables

#The tweets are represented by the 'text' variable, 
trump_tweets$text[16413] %>% str_wrap(width = options()$width) %>% cat
#'source' for device used to upload the tweets
trump_tweets %>% count(source) %>% arrange(desc(n)) %>% head(5)

#tweets between the day Trump announced his campaign and election day
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

#compute the proportion of tweets tweeted at each hour for each device
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

#Text data - how the tweets differ when we compare Android to iPhone
install.packages("tidytext")
library(tidytext)

#example token extrating
poem <- c("Roses are red,", "Violets are blue,", 
          "Sugar is sweet,", "And so are you.")
example <- tibble(line = c(1, 2, 3, 4),
                  text = poem)
example
example %>% unnest_tokens(word, text)

#let's extract tokens from the tweets - looking at tweet no 3008
i <- 3008
campaign_tweets$text[i] %>% str_wrap(width = 65) %>% cat()

campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  pull(word)

#use the tweets token including patterns that start with @ and #
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  pull(word)

# remove the links to pictures
links <- "https://t.co/[A-Za-z\\d]+|&amp;"
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  pull(word)

#extract the words for all our tweets
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") 

#what are the most commonly used words?
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

# The 'tidytext' package has database for stop words
stop_words

#filter out rows representing stop words
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word ) 

#we end up with a much more informative set of top 10 tweeted words
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

#generate final table removing unwanted charcters
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

#Odds ration of an Android tweet or an iPhone twee
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))

#the highest odds ratios for Android and iPhone
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

#impose a filter based on the total frequency
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)


#Sentiment Analysis
install.packages("textdata")
library(tidytext)
library(textdata)

#The bing lexicon divides words into positive and negative sentiments
get_sentiments("bing")
#The AFINN lexicon assigns a score between -5 and 5
get_sentiments("afinn")
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

# random words extracted from the tweets with sentiments
nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% 
  sample_n(5)

#count and compare the frequencies of each sentiment appearing in each device
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

#the odds ratio comparing the two devices
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

#an odds ratio and a confidence interval
library(broom)
log_or <- sentiment_counts %>%
  mutate(log_or = log((Android / (sum(Android) - Android)) / 
                        (iPhone / (sum(iPhone) - iPhone))),
         se = sqrt(1/Android + 1/(sum(Android) - Android) + 
                     1/iPhone + 1/(sum(iPhone) - iPhone)),
         conf.low = log_or - qnorm(0.975)*se,
         conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

#graphical visualization
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or)) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip()

#exploring which specific words are driving higher sentiment difference
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

#And we can make a graph
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 







