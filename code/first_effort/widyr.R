## correlations among sections

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



# 2. widyr ----------------------------------------------------------------

library(widyr)

# takes a while
word_pairs <- word_df %>%
  pairwise_count(word, document_number, sort = TRUE)

word_pairs <- word_pairs %>%
  filter(item1!="text", item2!="text")

word_cors <- word_df %>%
  filter(word!="text", word!="full", word!="caption", word!="photo") %>%
  group_by(word) %>%
  filter(n() >= 200) %>%
  pairwise_cor(word, document_number, sort = TRUE)

word_cors

word_cors %>%
  filter(correlation > .35) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
