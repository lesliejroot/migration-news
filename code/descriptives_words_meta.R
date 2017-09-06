library(tidyverse)
library(tidytext)

# 1. Load in meta data and create some new columns ------------------------

## load in all meta data from word counts

meta_word_df <- c()

for(i in 1:5){
  load(paste0("./data/meta_data_", i, ".Rda"))
  meta_word_df <- rbind(meta_word_df, all_meta_df)
}


## load in existing meta data that was pulled from ProQuest

csv_files <- list.files(path = "./data/raw",pattern = ".*.csv")
csv_files <- paste0("./data/raw/", csv_files)

meta_df <- c()

for(i in 1:length(csv_files)){
  this_meta <- read_csv(csv_files[i])
  meta_df <- bind_rows(meta_df, this_meta )
}


## join the two meta dfs

all_meta <- left_join(meta_df, meta_word_df, by = c("StoreId" = "id"))

# categorize article as either population or immigration-related based on the counts

all_meta <- all_meta %>% 
  mutate(document_category = case_when(
    immigra_count > population_count ~ "immigration",
    population_count > immigra_count ~ "population",
    TRUE ~ "both"
  ))

# categorize article type based on reduced categories 

table(all_meta$documentType)

all_meta <- all_meta %>% 
  mutate(document_type = case_when(
    documentType %in% c("News", "NEWSPAPER", "<none> , News") ~ "news",
    documentType %in% c("Commentary", "Editorial", "Op-Ed", "Opinions") ~ "commentary",
    documentType %in% c("Feature", "Series", "News , Series", "News; Series") ~ "feature/series",
    TRUE ~ "other"
  ))



# 2. Do some plots --------------------------------------------------------

## plot of document category by year

ggplot(all_meta %>% 
         group_by(year, document_category) %>% 
         summarise(n = n()), aes(year, n, color = document_category)) + 
  geom_line() + geom_point() + 
  scale_color_brewer(palette = "Set2") + 
  theme_bw()+
  ylab("number of articles")+
  ggtitle("Number of articles by category (based on more common keyword)")
ggsave("plots/category_year.pdf")

## plot of document type by year 

ggplot(all_meta %>%
         group_by(year, document_type) %>%
         summarise(n = n()), 
       aes(year, n, color = document_type))+
  geom_line() + geom_point() +
  scale_color_brewer(palette = "Set2", name = "Type") + 
  theme_bw()+
  ylab("Number")+
  ggtitle("Number of articles by document type")

## average word length

# by source
ggplot(all_meta %>%
         group_by(year, pubtitle) %>%
         summarise(ave_word = mean(word_count)), 
       aes(year, ave_word, color = pubtitle))+
  geom_line() + geom_point() +
  scale_color_brewer(palette = "Set1", name = "Source") + 
  theme_bw()+
  ylab("Average")+
  ggtitle("Average word count by source")
ggsave("plots/wordcount_source_year.pdf")

# by document_category

ggplot(all_meta %>%
         group_by(year, document_category) %>%
         summarise(ave_word = mean(word_count)), 
       aes(year, ave_word, color = document_category))+
  geom_line() + geom_point() +
  scale_color_brewer(palette = "Set2") + 
  theme_bw()+
  ylab("Average")+
  ggtitle("Average word count by category")
ggsave("plots/wordcount_category_year.pdf")

# by document_type
ggplot(all_meta %>%
         group_by(year, document_type) %>%
         summarise(ave_word = mean(word_count)) %>% 
         filter(document_type!="other"), 
       aes(year, ave_word, color = document_type))+
  geom_line() + geom_point() +
  scale_color_brewer(palette = "Set2", name = "Type") + 
  theme_bw()+
  ylab("Average")+
  ggtitle("Average word count by main document type")
ggsave("plots/wordcount_type_year.pdf")







