## Analysis of bigrams
library(tidyverse)
library(tidytext)
data("stop_words")

load(file = "./data/all_documents.Rda")


# 1. Get bigrams ----------------------------------------------------------

## Note this for-loop takes ages. better to load in processed file. 
# all_bigrams <- c()
# for(i in 1:length(all_documents)){
#   text <- all_documents[[i]]$text
#   text_df <- data_frame(paragraph = 1:length(text), text = text)
#   text_df$text[1] <- gsub("Full text: ", "", text_df$text[1])
#   
#   text_bigrams <- text_df %>% 
#     unnest_tokens(bigram, text, token = "ngrams", n = 2)
#   text_bigrams$document_number <- i
#   
#   all_bigrams <- rbind(all_bigrams, text_bigrams)
# }
# 
# save(all_bigrams, file = "./data/bigrams.Rda")


# 2. Load in meta data ----------------------------------------------------


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


# 3. Separate and clean words ---------------------------------------------


bigrams_separated <- all_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# unite back for future analysis
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")


# 4. Some word analysis ---------------------------------------------------

# what is immigrants precedeed by 
pre_immigrant <- bigrams_filtered %>%  
  filter(word2 == "immigrants"|word2 == "immigrant"|word2 == "migrants"|word2 == "migrant") %>%
  count(document_number, word1, sort = TRUE) %>%
  inner_join(all_meta %>% 
               select(year, StoreId, document_number))

im_year <- pre_immigrant %>%
  group_by(year, word1) %>% 
  summarise(n_year = sum(n)) %>%
  arrange(year, -n_year)

pd <- im_year %>%
  group_by(year) %>%
  top_n(10, wt = n_year) %>%
  ungroup() %>%
  arrange(year, n_year) %>%
  mutate(order=row_number())


ggplot(pd, aes(order, n_year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, scales = "free") +
  labs(y = "Instances",
       x = NULL) +
  scale_x_continuous(
    breaks = pd$order,
    labels = pd$word1,
    expand = c(0,0)
  ) +
  coord_flip()+
  ggtitle("Words preceeding immigrant(s) or migrant(s)")


