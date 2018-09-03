## topic models
library(tidyverse)
library(tidytext)
library(topicmodels)

# 1. Load in all the word tidy dfs ----------------------------------------
## NOTE TO SELF: make this a function?
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


all_meta <- all_meta %>%  group_by(StoreId) %>% 
  filter(row_number()==1) %>%  ungroup() %>% 
  mutate(date = mdy(pubdate),
         pres = ifelse(year(date)<2016, "Obama", "Trump")) %>% 
  mutate(two_digit_month = ifelse(nchar(month(date))==1, paste0(0, month(date)), as.character(month(date)))) %>% 
  mutate(mon_year = as.Date(paste(year(date), two_digit_month, "01", sep = "-"))) 

# filter so have same amount of days 
all_meta <- all_meta %>% 
  filter(!(date %in% min(all_meta$date[all_meta$pres=="Trump"]):ymd("2016-07-28")))

## now make the dates overlap by adding 8 years to obama
all_meta$date_adj <- all_meta$date
all_meta$date_adj[all_meta$pres=="Obama"] <- all_meta$date[all_meta$pres=="Obama"] + years(8)


# join to word df 
# NOTE: filtering out the art exhibitions. 
word_df <- word_df %>%
  inner_join(all_meta %>% select(document_number, StoreId, Title)) %>%
  filter(Title!= "The Listings: Art", Title!="Spare Times", Title!="Spare Time") %>%
  select(-Title)


# 2. Cast word df as a dtm ------------------------------------------------

# filter some obvious problems 
word_dtm_t <- word_df %>%
  left_join(all_meta %>% select(StoreId, pres)) %>% 
  filter(pres=="Trump") %>% 
  filter(is.na(as.numeric(word))) %>% 
  filter(word!="text", 
         word!="photo", word!="caption", word!="photograph", 
         word!="times", word!= "york", 
         word!= "a.m", word!="p.m",
         word!="immigration", word!="immigrants", word!="immigrant") %>%
  cast_dtm(document_number, word, n)

word_dtm_o <- word_df %>%
  left_join(all_meta %>% select(StoreId, pres)) %>% 
  filter(pres=="Obama") %>% 
  filter(is.na(as.numeric(word))) %>% 
  filter(word!="text", 
         word!="photo", word!="caption", word!="photograph", 
         word!="times", word!= "york", 
         word!= "a.m", word!="p.m",
         word!="immigration", word!="immigrants", word!="immigrant") %>%
  cast_dtm(document_number, word, n)


# 3. LDA ------------------------------------------------------------------

im_lda_t <- LDA(word_dtm_t, k = 8, control = list(seed = 1234))
im_lda_o <- LDA(word_dtm_o, k = 8, control = list(seed = 1234))

# betas are per-topic-per-word probabilities
im_topics_t <- tidy(im_lda_t, matrix = "beta")
im_topics_t

im_topics_o <- tidy(im_lda_o, matrix = "beta")
im_topics_o

## top betas
im_top_terms <- im_topics_t %>%
  group_by(topic) %>%
  filter(term!="text", term!="york") %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

im_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()+
  theme_bw()+
  ggtitle("Top words in a eight topic model: Trump")
ggsave("./plots/tm_8_topic_t.pdf", width = 10, height = 8)

## greatest difference in  ββ  between topic 1 and topic 2. 

beta_spread <- im_topics_o %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  arrange(log_ratio)

im_top_diff <- beta_spread %>%
  top_n(40, abs(log_ratio)) %>%
  arrange(log_ratio)

im_top_diff %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio))+
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_bw() + ggtitle("Most different words, two topic model")
#ggsave("./plots/tm_diff_2topic.pdf", width = 10, height = 7)




## find articles that match these topics?


im_gamma <- tidy(im_lda_o, matrix = "gamma")


example_docs <- im_gamma %>% 
  group_by(topic) %>%
  top_n(30, gamma) %>%
  mutate(document_number = as.numeric(document)) %>%
  inner_join(all_meta %>% select(document_number, year, StoreId, Title, Abstract, subjectTerms)) %>%
  arrange(topic, -gamma)

write_csv(example_docs, "./tables/example_documents_8topic_o.csv")


# categorise each document based on max probability

max_gamma <- im_gamma %>% 
  group_by(document) %>%
  filter(gamma == max(gamma)) %>%
  ungroup() %>%
  mutate(document = as.numeric(document)) %>%
  arrange(document)

# most probabilities are above 0.5
hist(max_gamma$gamma)
abline(v = 0.5)

doc_counts <- all_meta %>% group_by(year) %>% summarise(n_doc = n())

max_gamma %>%
  mutate(document_number = as.numeric(document)) %>%
  inner_join(all_meta %>% select(document_number, year, mon_year, StoreId, Title, Abstract, subjectTerms)) %>%
  inner_join(doc_counts) %>%
  group_by(mon_year, topic) %>%
  summarise(n_doc_topic = n(), prop_doc_topic = n_doc_topic/min(n_doc)) %>%
  mutate(topic_factor = factor(topic, levels = 1:8, labels = c("Terrorism", "Economy", "Europe", "Art/museums", "DACA/bills", "Election", "Family Separation", "ICE"))) %>%
  #filter(topic %in% c(2,4,6,7,8)) %>%
  ggplot(aes(mon_year, prop_doc_topic, color = topic_factor)) + 
  geom_line() + geom_point()+theme_bw()+
  ggtitle("Proportion of documents in top eight topics: Trump")+
  scale_color_discrete(name = "Topic")
ggsave("./plots/tm_doc_month_8_t.pdf", width = 10, height = 7)


max_gamma %>%
  mutate(document_number = as.numeric(document)) %>%
  inner_join(all_meta %>% select(document_number, year, mon_year, StoreId, Title, Abstract, subjectTerms)) %>%
  inner_join(doc_counts) %>%
  group_by(mon_year, topic) %>%
  summarise(n_doc_topic = n(), prop_doc_topic = n_doc_topic/min(n_doc)) %>%
  mutate(topic_factor = factor(topic, levels = 1:8, labels = c("Election", "Plays", "Economy", "Illegal immigrants", "Terrorism", "Cities/minorities",
                                                               "Arts/museums", "Obits?"))) %>%
  #filter(topic %in% c(2,4,6,7,8)) %>%
  ggplot(aes(mon_year, prop_doc_topic, color = topic_factor)) + 
  geom_line() + geom_point()+theme_bw()+
  ggtitle("Proportion of documents in top eight topics: Obama")+
  scale_color_discrete(name = "Topic")
ggsave("./plots/tm_doc_month_8_o.pdf", width = 10, height = 7)

