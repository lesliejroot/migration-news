library(tidyverse)
library(tidytext)


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

ggplot(all_meta %>% 
         group_by(year, document_category) %>% 
         summarise(n = n()), aes(year, n, color = document_category)) + 
  geom_line() + geom_point() + 
  scale_color_brewer(palette = "Set2") + 
  theme_bw()+
  ylab("number of articles")+
  ggtitle("Number of articles by category (based on more common keyword)")
ggsave("plots/category_year.pdf")
