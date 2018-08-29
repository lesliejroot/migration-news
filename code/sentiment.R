library(tidyverse)
library(tidytext)
library(lubridate)


# 1. Load in all the word tidy dfs ----------------------------------------

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

all_meta <- all_meta %>% 
  mutate(date = mdy(pubdate)) %>% 
  mutate(two_digit_month = ifelse(nchar(month(date))==1, paste0(0, month(date)), as.character(month(date)))) %>% 
  mutate(mon_year = as.Date(paste(year(date), two_digit_month, "01", sep = "-"))) 

# remove duplicate store ids

all_meta <- all_meta %>%  group_by(StoreId) %>% 
  filter(row_number()==1) %>%  ungroup()

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
               select(date, StoreId, word_count)) %>%
  mutate(prop_sent = n_sent/word_count) %>%
  group_by(date, sentiment) %>%
  summarise(ave_prop_sent = mean(prop_sent))


# not sure there's much here
ggplot(nrc_words, aes(date, ave_prop_sent)) + facet_wrap(~sentiment) + geom_point()

# what happened with that spike?

nrc_words %>% arrange(-ave_prop_sent)


all_meta %>%  filter(date == ymd("2017-12-14")) %>%  select(Title)
# all_meta %>%  filter(date == ymd("2017-12-14")) %>% 
#   left_join(word_df) %>% 
#   inner_join(get_sentiments("nrc")) %>% 
#   group_by(StoreId, sentiment) %>% 
#   summarise(n_sent = sum(n)) %>% arrange(-n_sent)


# do the same thing for bing categories
# NOTE NEED TO REMOVE 'trump' from positive words 

bing_words <- word_df %>%
  inner_join(get_sentiments("bing")) %>%
  filter(word!="trump") %>%
  group_by(document_number, StoreId, sentiment) %>%
  summarise(n_sent = sum(n)) %>%
  inner_join(all_meta %>% 
               select(date, StoreId, word_count)) %>%
  mutate(prop_sent = n_sent/word_count) %>%
  group_by(date, sentiment) %>%
  summarise(ave_prop_sent = mean(prop_sent))

ggplot(bing_words, aes(date, ave_prop_sent)) + facet_grid(.~sentiment) + geom_line()


all_meta %>%  filter(date == ymd("2017-12-14")) %>%
  left_join(word_df) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(StoreId, Title, sentiment) %>%
  summarise(n_sent = sum(n)) %>% arrange(-n_sent)

# 3. top words ------------------------------------------------------------


# look at top positive and negative words by year?

word_sent_year <- word_df %>%
  inner_join(get_sentiments("bing")) %>%
  filter(word!="trump") %>%
  inner_join(all_meta %>% 
               select(mon_year, StoreId, word_count)) %>%
  group_by(word, mon_year, sentiment) %>%
  summarise(n_word = sum(n), prop_word = n_word/sum(word_count)) %>%
  arrange(mon_year, -n_word)
  
main_sent_words <- word_sent_year %>%
  filter(word!="illegal") %>% 
  group_by(mon_year, sentiment) %>%
  #group_by(sentiment) %>%
  top_n(n = 2, wt = n_word) %>%
  ungroup() %>%
  select(word)

ggplot(word_sent_year %>% filter(word %in% main_sent_words[[1]],  word!="issues", word!="hard",
                                 sentiment=="negative"), aes(mon_year, prop_word, color = word)) + 
  geom_line()+ geom_point() + theme_bw()+
  ggtitle("Top negative words") + ylab("Proprtion of all words")
#ggsave("./plots/sent_neg_year.pdf", height = 7, width = 10)

ggplot(word_sent_year %>% filter(word %in% main_sent_words[[1]],  word!="win",
                                 sentiment=="positive"), aes(mon_year, prop_word, color = word)) + 
  geom_line()+ geom_point() + theme_bw()+
  ggtitle("Top positive words") + ylab("Proprtion of all words")
#ggsave("./plots/sent_pos_year.pdf", height = 7, width = 10)

## might be nice to see as a bar plot

pd <- word_sent_year %>%
  group_by(sentiment, mon_year) %>%
  top_n(10, wt = n_word) %>%
  ungroup() %>%
  arrange(mon_year, sentiment, n_word) %>%
  mutate(order=row_number())


  ggplot(pd, aes(order, n_word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~mon_year, scales = "free") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
    scale_x_continuous(
      breaks = pd$order,
      labels = pd$word,
      expand = c(0,0)
    ) +
  coord_flip()+
    ggtitle("Top positive and negative words by year")
#ggsave("plots/sent_words_year.pdf", width = 12, height = 10)




# 4. Total sentiment over time --------------------------------------------


## total sentiment scores by each year for each of the methods

afinn <- word_df %>% 
  inner_join(get_sentiments("afinn")) %>% 
  filter(word!="trump", word!="united") %>%
  inner_join(all_meta %>% 
               select(date, StoreId, word_count)) %>%
  group_by(date) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN", index = date) 

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
               select(date, StoreId, word_count)) %>%
  count(method, index = date, sentiment) %>%
  spread(sentiment, nn, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, color = method)) + geom_line()

afinn %>% 
  ggplot(aes(index, sentiment)) + 
  geom_line() + #geom_point()+
  theme_bw() + xlab("date") +
  ggtitle("Sentiment over time (AFINN method)")
ggsave("./plots/sentiment_year.pdf", height = 7, width = 10)




# 5. Tf_idf ---------------------------------------------------------------

word_df <- word_df %>% 
  bind_tf_idf(word, StoreId, n)

tfidf <- word_df %>% 
  inner_join(all_meta %>% 
               select(mon_year, StoreId)) %>%
  group_by(mon_year, word) %>% 
  summarise(sum_tfidf = sum(tf_idf))

pd <- tfidf %>%
  group_by(mon_year) %>%
  top_n(10, wt = sum_tfidf) %>%
  ungroup() %>%
  arrange(mon_year, sum_tfidf) %>%
  mutate(order=row_number())


ggplot(pd, aes(order, sum_tfidf)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~mon_year, scales = "free") +
  labs(y = "tf_idf (summed across all documents)",
       x = NULL) +
  scale_x_continuous(
    breaks = pd$order,
    labels = pd$word,
    expand = c(0,0)
  ) +
  coord_flip()+
  ggtitle("Important words by month")
  
ggsave("plots/tf_idf_month_barplot.pdf", width = 15, height = 10)

# do with daily data but just top 10 words overall
tfidf <- word_df %>% 
  inner_join(all_meta %>% 
               select(date, StoreId)) %>%
  group_by(date, word) %>% 
  summarise(sum_tfidf = sum(tf_idf))


top_words <- tfidf %>%
  filter(word!="ms", word!="trump's", word!="united") %>% 
  group_by(word) %>% 
  summarise(total_tfidf = sum(sum_tfidf)) %>% 
  arrange(-total_tfidf) %>% 
  top_n(10, wt = total_tfidf) %>% 
  select(word) %>%  pull()

pd <- tfidf %>%
  filter(word %in% top_words) %>% 
  ungroup() %>%
  arrange(date, sum_tfidf) 

pd %>% 
  group_by(word) %>% 
  mutate(csum = cumsum(sum_tfidf)) %>% 
  ggplot(aes(date, csum, color = word)) + geom_line(lwd = 1.2)

