library(tidyverse)
library(tidytext)


# 1. Load in all the word tidy dfs ----------------------------------------


word_df <- c()

for(i in 1:5){
  load(paste0("./data/word_counts_", i, ".Rda"))
  word_df <- rbind(word_df, all_word_counts_df)
}

## also load in meta data to get document IDs

## load in all meta data from word counts

meta_word_df <- c()

for(i in 1:5){
  load(paste0("./data/meta_data_", i, ".Rda"))
  meta_word_df <- rbind(meta_word_df, all_meta_df)
}


word_df <- word_df %>%
  inner_join(all_meta %>% select(document_number, StoreId))


# 2. Exploring some basic sentiment analysis ------------------------------

get_sentiments("bing") %>% 
  group_by(sentiment) %>% 
  summarize(n = n())

nrcneg <- get_sentiments("bing") %>%
  filter(sentiment=="negative")

n_neg_words <- word_df %>%
  inner_join(nrcneg) %>%
  group_by(document_number, StoreId) %>%
  summarise(n_neg = sum(n))

## plot proportion of negative words over time

ave_prop_neg <- n_neg_words %>%
  inner_join(all_meta %>% 
               select(year, StoreId, word_count)) %>%
  mutate(prop_neg = n_neg/word_count) %>%
  group_by(year) %>%
  summarise(ave_neg_prop = mean(prop_neg), se_neg = sqrt(var(prop_neg)))


ggplot(ave_prop_neg, 
  aes(year, ave_neg_prop)) +
    geom_line() + geom_point()+ 
  #geom_ribbon(aes(ymax = ave_neg_prop+se_neg, ymin = ave_neg_prop-se_neg), alpha = 0.5)+
  theme_bw()+
  ggtitle("Average proportion of negative words per article")



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
  top_n(n = 5, wt = n_word) %>%
  ungroup() %>%
  select(word)

ggplot(word_sent_year %>% filter(word %in% main_sent_words[[1]],  
                                 sentiment=="negative"), aes(year, prop_word, color = word)) + 
  geom_line()

ggplot(word_sent_year %>% filter(word %in% main_sent_words[[1]],  
                                 sentiment=="positive"), aes(year, prop_word, color = word)) + 
  geom_line()


##

afinn <- word_df %>% 
  inner_join(get_sentiments("afinn")) %>% 
  filter(word!="trump") %>%
  inner_join(all_meta %>% 
               select(year, StoreId, word_count)) %>%
  group_by(year) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN", index = year) 

bing_and_nrc <- bind_rows(word_df %>% 
                            inner_join(get_sentiments("bing")) %>%
                            filter(word!="trump") %>%
                            mutate(method = "Bing et al."),
                          word_df %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"),word!="trump")) %>%
                            mutate(method = "NRC")) %>%
  inner_join(all_meta %>% 
               select(year, StoreId, word_count)) %>%
  count(method, index = year, sentiment) %>%
  spread(sentiment, nn, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, color = method)) + geom_line()
