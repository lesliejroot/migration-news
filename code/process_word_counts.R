library(tidyverse)
library(tidytext)
library(foreach)
library(doParallel) 
data("stop_words")
source("./code/functions/get_word_counts_meta.R")


load(file = "./data/all_documents.Rda")

## want to make a big df with word counts by document
## also keep track of meta data

## split up this into chunks, not sure how big these word count dfs are going to be 

start_chunk <- c(1, 2000, 4000, 6000, 8000)
end_chunk <- c(1999, 3999, 5999, 7999, length(all_documents))

registerDoParallel(cores=2)  
getDoParWorkers() 

foreach(i=1:length(start_chunk)) %dopar% {
  cat(paste("Starting word counts for documents", start_chunk[i], "to", end_chunk[i], "./n"))
  all_word_counts_df <- c()
  all_meta_df <- c()
  for(j in start_chunk[i]:end_chunk[i]){
    res <- get_word_counts_meta(all_documents[[j]])
    
    word_counts_df <- res$word_counts_df
    meta_df <- res$meta_df
    
    word_counts_df$document_number <- j
    meta_df$document_number <- j
    
    all_word_counts_df <- rbind(all_word_counts_df, word_counts_df)
    all_meta_df <- rbind(all_meta_df, meta_df)
  }
  save(all_word_counts_df, file = paste0("./data/word_counts_",i,".Rda"))
  save(all_meta_df, file = paste0("./data/meta_data_",i,".Rda"))
}


