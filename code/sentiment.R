library(tidyverse)
library(tidytext)
library(lubridate)


# 1. Load in all the word tidy dfs ----------------------------------------

word_df <- c()

for(i in 1:9){
  load(paste0("./data/word_counts_", i, ".Rda"))
  word_df <- rbind(word_df, all_word_counts_df)
}
rm(all_word_counts_df)

## also load in meta data to get document IDs

## load in all meta data from word counts

meta_word_df <- c()

for(i in 1:9){
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
  mutate(date = mdy(pubdate))

# remove duplicate store ids
# add some info about presidents

all_meta <- all_meta %>%  group_by(StoreId) %>% 
  filter(row_number()==1) %>%  ungroup() %>% 
  mutate(date = mdy(pubdate),
         pres = ifelse(year(date)<2016, "Obama", "Trump")) 

# filter so have same amount of days 
all_meta <- all_meta %>% 
  filter(!(date %in% min(all_meta$date[all_meta$pres=="Trump"]):ymd("2016-07-28")))
  
## now make the dates overlap by adding 8 years to obama
all_meta$date_adj <- all_meta$date
all_meta$date_adj[all_meta$pres=="Obama"] <- all_meta$date[all_meta$pres=="Obama"] + years(8)


all_meta <- all_meta %>% 
  mutate(two_digit_month = ifelse(nchar(month(date_adj))==1, paste0(0, month(date_adj)), as.character(month(date_adj)))) %>% 
  mutate(mon_year = as.Date(paste(year(date_adj), two_digit_month, "01", sep = "-"))) 
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
               select(date_adj, StoreId, word_count, pres)) %>%
  mutate(prop_sent = n_sent/word_count) %>%
  group_by(date_adj, sentiment, pres) %>%
  summarise(ave_prop_sent = mean(prop_sent))


# not sure there's much here
ggplot(nrc_words, aes(date_adj, ave_prop_sent, color = pres)) + facet_wrap(~sentiment) + geom_point()

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
               select(date_adj, StoreId, word_count, pres)) %>%
  mutate(prop_sent = n_sent/word_count) %>%
  group_by(date_adj, pres, sentiment) %>%
  summarise(ave_prop_sent = mean(prop_sent))

ggplot(bing_words, aes(date_adj, ave_prop_sent, color = pres )) + facet_grid(.~sentiment) + geom_line()


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
               select(date_adj, StoreId, word_count, pres)) %>%
  group_by(date_adj, pres) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN", index = date_adj) 

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
  ggplot(aes(index, sentiment, color = pres, fill = pres)) + 
  geom_line() + 
  #geom_point(alpha = 0.3)+
  theme_classic(base_size = 14) + xlab("date") +
  scale_color_brewer(name = "president", palette = "Set1", direction = -1) +
  scale_fill_brewer(name = "president", palette = "Set1", direction = -1) +
  ggtitle("Average daily sentiment")+ 
  geom_vline(xintercept = mdy("09-17-2016"), color = "darkblue", lty = 2) +
  annotate("text", x=mdy("08-15-2016"), y=150, label= "Chelsea \nbombing", color = "darkblue", size = 5) +
  geom_vline(xintercept = mdy("01-27-2017"), color = "orange", lty = 2) +
  annotate("text", x=mdy("12-20-2016"), y=160, label= "travel ban", color = "orange", size = 5) +
  geom_vline(xintercept = mdy("10-31-2017"), color = "darkgreen", lty = 2) +
  annotate("text", x=mdy("09-20-2017"), y=160, label= "New York\n van attack", color = "darkgreen", size = 5) +
  geom_vline(xintercept = mdy("06-15-2018"), color = "purple", lty = 2) +
  annotate("text", x=mdy("05-20-2018"), y=160, label= "family \n separation", color = "purple", size = 5) 
ggsave("./plots/sentiment_day_pres.pdf", height = 7, width = 12)
# save the results

write_csv(afinn %>% select(date_adj, pres, sentiment), "./output/sentiment_day_pres.csv")

afinn <- word_df %>% 
  inner_join(get_sentiments("afinn")) %>% 
  filter(word!="trump", word!="united", word!= "win", word!="won") %>%
  inner_join(all_meta %>% 
               select(mon_year, StoreId, word_count, pres)) %>%
  group_by(mon_year, pres) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN", index = mon_year) 

afinn %>% 
  ggplot(aes(index, sentiment, color = pres)) + 
  geom_line() + geom_point()+
  theme_bw() + xlab("date") +
  scale_color_brewer(name = "president", palette = "Set1", direction = -1) + 
  ggtitle("Average monthly sentiment")
ggsave("./plots/sentiment_month_pres.pdf", height = 7, width = 12)

# 5. Tf_idf ---------------------------------------------------------------

word_df <- word_df %>% 
  bind_tf_idf(word, StoreId, n)

tfidf <- word_df %>% 
  inner_join(all_meta %>% 
               select(mon_year, pres, StoreId)) %>%
  filter(pres=="Trump") %>% 
  group_by(mon_year, word) %>% 
  summarise(sum_tfidf = sum(tf_idf))

pd <- tfidf %>%
  group_by(mon_year) %>%
  filter(word!="trump's") %>% 
  top_n(10, wt = sum_tfidf) %>%
  ungroup() %>%
  arrange(mon_year, sum_tfidf) %>%
  mutate(order=row_number())

pd %>% 
  filter(mon_year %in% c(ymd("2016-08-01"), ymd("2017-01-01"), ymd("2017-09-01"), ymd("2018-06-01"))) %>% 
  mutate(month_year = case_when(
                      mon_year == ymd("2016-08-01") ~ "2016-08",
                      mon_year == ymd("2017-01-01") ~ "2017-01",
                      mon_year == ymd("2017-09-01") ~ "2017-09",
                      mon_year == ymd("2018-06-01") ~ "2018-06",
                      TRUE ~ "NA"
                     )) %>% 
  
ggplot(aes(order, sum_tfidf, fill = month_year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~month_year, scales = "free") +
  labs(y = "tf_idf (summed across all documents)",
       x = NULL) +
  scale_x_continuous(
    breaks = pd$order,
    labels = pd$word,
    expand = c(0,0)
  ) +
  coord_flip()+
  scale_fill_viridis_d()+
  theme_bw(base_size = 14)
  
ggsave("plots/tf_idf_month_barplot_select.pdf", width = 12, height = 10)

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

