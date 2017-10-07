library(tidyverse)
library(tidytext)


# 1. Load in all the word tidy dfs ----------------------------------------
## NOTE TO SELF: make this a function?


word_df <- c()

for(i in 1:5){
  load(paste0("./data/word_counts_", i, ".Rda"))
  word_df <- rbind(word_df, all_word_counts_df)
}
rm(all_word_counts_df)

## also load in meta data to get document IDs

## load in all meta data from word counts

meta_word_df <- c()

for(i in 1:5){
  load(paste0("./data/meta_data_", i, ".Rda"))
  meta_word_df <- rbind(meta_word_df, all_meta_df)
}
rm(all_meta_df)

## load in existing meta data that was pulled from ProQuest

csv_files <- list.files(path = "./data/raw",pattern = ".*.csv")
csv_files <- paste0("./data/raw/", csv_files)

meta_df <- c()

for(i in 1:length(csv_files)){
  this_meta <- read_csv(csv_files[i])
  meta_df <- bind_rows(meta_df, this_meta )
}
rm(this_meta)

## join the two meta dfs

all_meta <- left_join(meta_df, meta_word_df, by = c("StoreId" = "id"))

rm(meta_df, meta_word_df)

# categorize article as either population or immigration-related based on the counts
# also categorize years into broader periods
all_meta <- all_meta %>% 
  mutate(document_category = case_when(
    immigra_count > population_count ~ "immigration",
    population_count > immigra_count ~ "population",
    TRUE ~ "both"
  ),
  period = case_when(
    year < 2012 ~ "2008-2011",
    year < 2016 & year > 2011 ~ "2012-2015",
    TRUE ~ "2016-2017"
  ))




# join to word df 
word_df <- word_df %>%
  inner_join(all_meta %>% select(document_number, StoreId))


# 2. Exploring some basic sentiment analysis ------------------------------

# try and do it for all nrc categories

nrc_words <- word_df %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(document_number, StoreId, sentiment) %>%
  summarise(n_sent = sum(n)) %>%
  inner_join(all_meta %>% 
               select(year, StoreId, word_count)) %>%
  mutate(prop_sent = n_sent/word_count) %>%
  group_by(year, sentiment) %>%
  summarise(ave_prop_sent = mean(prop_sent))


# not sure there's much here
ggplot(nrc_words, aes(year, ave_prop_sent)) + facet_grid(.~sentiment) + geom_line()


# do the same thing for bing categories
# NOTE NEED TO REMOVE 'trump' from positive words 

bing_words <- word_df %>%
  inner_join(get_sentiments("bing")) %>%
  filter(word!="trump") %>%
  group_by(document_number, StoreId, sentiment) %>%
  summarise(n_sent = sum(n)) %>%
  inner_join(all_meta %>% 
               select(year, StoreId, word_count)) %>%
  mutate(prop_sent = n_sent/word_count) %>%
  group_by(year, sentiment) %>%
  summarise(ave_prop_sent = mean(prop_sent))

ggplot(bing_words, aes(year, ave_prop_sent)) + facet_grid(.~sentiment) + geom_line()



# 3. top words ------------------------------------------------------------


# look at top positive and negative words by year?

word_sent_year <- word_df %>%
  inner_join(get_sentiments("bing")) %>%
  filter(word!="trump") %>%
  inner_join(all_meta %>% 
               select(year, StoreId, word_count)) %>%
  group_by(word, year, sentiment) %>%
  summarise(n_word = sum(n), prop_word = n_word/sum(word_count)) %>%
  arrange(year, -n_word)
  
main_sent_words <- word_sent_year %>%
  group_by(year, sentiment) %>%
  #group_by(sentiment) %>%
  top_n(n = 2, wt = n_word) %>%
  ungroup() %>%
  select(word)

ggplot(word_sent_year %>% filter(word %in% main_sent_words[[1]],  word!="issues",
                                 sentiment=="negative"), aes(year, prop_word, color = word)) + 
  geom_line()+ geom_point() + theme_bw()+
  ggtitle("Top negative words") + ylab("Proprtion of all words")
ggsave("./plots/sent_neg_year.pdf", height = 7, width = 10)

ggplot(word_sent_year %>% filter(word %in% main_sent_words[[1]],  
                                 sentiment=="positive"), aes(year, prop_word, color = word)) + 
  geom_line()

## might be nice to see as a bar plot

pd <- word_sent_year %>%
  group_by(sentiment, year) %>%
  top_n(10, wt = n_word) %>%
  ungroup() %>%
  arrange(year, sentiment, n_word) %>%
  mutate(order=row_number())


  ggplot(pd, aes(order, n_word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~year, scales = "free") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
    scale_x_continuous(
      breaks = pd$order,
      labels = pd$word,
      expand = c(0,0)
    ) +
  coord_flip()+
    ggtitle("Top positive and negative words by year")
ggsave("plots/sent_words_year.pdf", width = 12, height = 10)

## do the same thing by period

word_sent_period <- word_df %>%
  inner_join(get_sentiments("bing")) %>%
  filter(word!="trump") %>%
  inner_join(all_meta %>% 
               select(period, StoreId, word_count)) %>%
  group_by(word, period, sentiment) %>%
  summarise(n_word = sum(n), prop_word = n_word/sum(word_count)) %>%
  arrange(period, -n_word)

pd <- word_sent_period %>%
  group_by(sentiment, period) %>%
  top_n(10, wt = n_word) %>%
  ungroup() %>%
  arrange(period, sentiment, n_word) %>%
  mutate(order=row_number())


ggplot(pd, aes(order, n_word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~period, scales = "free") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  scale_x_continuous(
    breaks = pd$order,
    labels = pd$word,
    expand = c(0,0)
  ) +
  coord_flip()+
  ggtitle("Top positive and negative words by period")
ggsave("plots/sent_words_period.pdf", width = 12, height = 10)



# 4. Total sentiment over time --------------------------------------------


## total sentiment scores by each year for each of the methods

afinn <- word_df %>% 
  inner_join(get_sentiments("afinn")) %>% 
  filter(word!="trump", word!="united") %>%
  inner_join(all_meta %>% 
               select(year, StoreId, word_count)) %>%
  group_by(year) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN", index = year) 

bing_and_nrc <- bind_rows(word_df %>% 
                            inner_join(get_sentiments("bing")) %>%
                            filter(word!="trump", word!="united") %>%
                            mutate(method = "Bing et al."),
                          word_df %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"),word!="trump", word!="united")) %>%
                            mutate(method = "NRC")) %>%
  inner_join(all_meta %>% 
               select(year, StoreId, word_count)) %>%
  count(method, index = year, sentiment) %>%
  spread(sentiment, nn, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, color = method)) + geom_line()

afinn %>% 
  ggplot(aes(index, sentiment)) + 
  geom_line() + geom_point()+
  theme_bw() + xlab("year") +
  ggtitle("Sentiment over time (AFINN method)")
ggsave("./plots/sentiment_year.pdf", height = 7, width = 10)




# 5. Tf_idf ---------------------------------------------------------------

word_df <- word_df %>% 
  bind_tf_idf(word, StoreId, n)

tfidf <- word_df %>% 
  inner_join(all_meta %>% 
               select(year, StoreId)) %>%
  group_by(year, word) %>% 
  summarise(sum_tfidf = sum(tf_idf))

pd <- tfidf %>%
  group_by(year) %>%
  top_n(10, wt = sum_tfidf) %>%
  ungroup() %>%
  arrange(year, sum_tfidf) %>%
  mutate(order=row_number())


ggplot(pd, aes(order, sum_tfidf)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, scales = "free") +
  labs(y = "tf_idf (summed across all documents)",
       x = NULL) +
  scale_x_continuous(
    breaks = pd$order,
    labels = pd$word,
    expand = c(0,0)
  ) +
  coord_flip()+
  ggtitle("Important words by year")
  
ggsave("plots/tf_idf_year_barplot.pdf", width = 15, height = 10)
