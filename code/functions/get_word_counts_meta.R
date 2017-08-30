get_word_counts_meta <- function(text_id_list # must be a list with a text element and storeID element
                                 ){
  text <- text_id_list$text
  text_df <- data_frame(paragraph = 1:length(text), text = text)
  meta_df <- data.frame(id = text_id_list$storeID)
  
  tidy_text <- text_df %>% 
    unnest_tokens(word, text)
  
  ## do a word count, keep in metadata
  meta_df$word_count <- nrow(tidy_text)
  
  # take out the joining words and count word incidences
  tidy_text <- tidy_text %>% anti_join(stop_words)
  word_counts <- tidy_text %>%
    count(word, sort = TRUE) 
  
  meta_df$immigra_count <- length(grep("immigra*", word_counts$word))
  meta_df$population_count <- length(grep("population", word_counts$word))
  
  return(list(word_counts_df = word_counts, 
              meta_df = meta_df
              ))
}