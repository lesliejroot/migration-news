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

im_totals <- im_year %>%
  group_by(year) %>%
  summarise(n_immi = sum(n_year))

pd <- im_year %>%
  inner_join(im_totals) %>%
  group_by(year) %>%
  top_n(10, wt = n_year) %>%
  ungroup() %>%
  arrange(year, n_year) %>%
  mutate(prop_word = n_year/n_immi, 
         order=row_number())


ggplot(pd, aes(order, prop_word)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, scales = "free") +
  labs(y = "Proportion of total instances",
       x = NULL) +
  scale_x_continuous(
    breaks = pd$order,
    labels = pd$word1,
    expand = c(0,0)
  ) +
  coord_flip()+
  ggtitle("Words preceeding immigrant(s) or migrant(s)")
ggsave("plots/bigram_immigration_year_barplot.pdf", width = 15, height = 10)


# maybe do a line plot with common words. 
# note "illegal" needs to be done separately
immi_words_negative <- c( "illegal", "undocumented","unauthorized", "anti")
immi_words_positive <- c("legal", "recent", "skilled")
immi_words_eth <- c("mexican",  "muslim", "chinese", "asian", "latino", "hispanic")


im_year %>%
  inner_join(im_totals) %>%
  filter(word1 %in% immi_words_negative) %>%
  mutate(prop_word = n_year/n_immi) %>%
  ggplot(aes(year, prop_word, color = word1)) + geom_point() + geom_line()+
  theme_bw() + ggtitle("Negative words preceeding immigrant/migrant") + ylab("Proportion of instances")
ggsave("plots/bigram_immigration_neg_year.pdf", width = 10, height = 7)

im_year %>%
  inner_join(im_totals) %>%
  filter(word1 %in% immi_words_positive) %>%
  mutate(prop_word = n_year/n_immi) %>%
  ggplot(aes(year, prop_word, color = word1)) + geom_point() + geom_line()+
  theme_bw() + ggtitle("Positive words preceeding immigrant/migrant") + ylab("Proportion of instances")
ggsave("plots/bigram_immigration_pos_year.pdf", width = 10, height = 7)

im_year %>%
  inner_join(im_totals) %>%
  filter(word1 %in% immi_words_eth) %>%
  mutate(prop_word = n_year/n_immi) %>%
  ggplot(aes(year, prop_word, color = word1)) + geom_point() + geom_line()+
  theme_bw() + ggtitle("Ethnicities preceeding immigrant/migrant") + ylab("Proportion of instances")
ggsave("plots/bigram_immigration_eth_year.pdf", width = 10, height = 7)



# 5. Tf_idf ---------------------------------------------------------------

bigram_tf_idf <- bigrams_united %>%
  count(document_number, bigram) %>%
  bind_tf_idf(bigram, document_number, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>% 
  group_by(bigram) %>% 
  summarise(sum_tf_idf = sum(tf_idf)) %>%
  arrange(-sum_tf_idf)


# 6. Sentiment ------------------------------------------------------------

AFINN <- get_sentiments("afinn")
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip() + theme_bw()
ggsave("plots/bigram_not_barplot.pdf", width = 10, height = 7)


trump_words <- bigrams_separated %>%
  filter(word1 == "trump") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

trump_words
trump_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"Trump\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip() + theme_bw()
ggsave("plots/bigram_trump_barplot.pdf", width = 10, height = 7)


obama_words <- bigrams_separated %>%
  filter(word1 == "obama") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

obama_words
obama_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"Obama\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip() + theme_bw()
ggsave("plots/bigram_obama_barplot.pdf", width = 10, height = 7)


immigrant_words <- bigrams_separated %>%
  filter(word1 == "immigrants") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

immigrant_words
immigrant_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"immigrant\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip() + theme_bw()
ggsave("plots/bigram_immigrant_barplot.pdf", width = 10, height = 7)



# 7. graphing -------------------------------------------------------------

library(igraph)
library(ggraph)

bigram_graph <- bigram_counts %>%
  filter(n > 220) %>%
  graph_from_data_frame()

bigram_graph
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_bw()

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
ggsave("./plots/bigram_network.pdf", width = 15, height = 15)
