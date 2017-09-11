### author analysis

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





# 2. Clean author column ---------------------------------------------------

all_meta <- all_meta %>%
  mutate(fauthor = gsub("\\r.*", "", Authors))

# 2505 unique authors
length(unique(all_meta$fauthor))

# average number of articles per author
all_meta %>% 
  filter(fauthor!= "Anonymous", !is.na(fauthor)) %>%
  group_by(year) %>%
  summarise(n_articles = n(),
    n_auth = n_distinct(fauthor),
    ave_art = n_articles/n_auth) %>%
  ggplot(aes(year, ave_art)) + 
  geom_point() + geom_line() + 
  theme_bw() + ggtitle("Average number of articles per author")
ggsave("./plots/ave_articles_author.pdf")



# number of articles by each author
all_meta %>%
  filter(fauthor!= "Anonymous", !is.na(fauthor)) %>%
  group_by(fauthor) %>%
  summarise(n_art = n()) %>%
  arrange(-n_art)
