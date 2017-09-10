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

all_meta <- all_meta %>% 
  mutate(document_category = case_when(
    immigra_count > population_count ~ "immigration",
    population_count > immigra_count ~ "population",
    TRUE ~ "both"
  ))


# join to word df 
word_df <- word_df %>%
  inner_join(all_meta %>% select(document_number, StoreId))




# 2. Trump v not ----------------------------------------------------------

## work out whether document contains trump or not

word_df <- word_df %>%
  group_by(StoreId) %>%
  mutate(mention_trump = (any(word=="trump")&any(word=="donald")))


# number of articles that mention trump
# can only do this from 2015
word_df %>% group_by(StoreId) %>%
  summarise(mt = mean(mention_trump)) %>%
  inner_join(all_meta %>% 
               select(year, StoreId, word_count)) %>%
  group_by(mt, year) %>%
  summarise(n())


# simple sentiment by trump v not 

afinn <- word_df %>% 
  inner_join(get_sentiments("afinn")) %>% 
  filter(word!="trump") %>%
  inner_join(all_meta %>% 
               select(year, StoreId, word_count)) %>%
  group_by(year, mention_trump, StoreId) %>% 
  summarise(sentiment = sum(score)) %>%
  group_by(year, mention_trump) %>%
  summarise(ave_sentiment = mean(sentiment),
            sd_sentiment = sd(sentiment)) %>% 
  mutate(method = "AFINN", index = year) 

ggplot(afinn %>% filter(year>2014), 
       aes(year, ave_sentiment, color = mention_trump)) + geom_line()
  