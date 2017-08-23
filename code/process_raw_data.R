library(tidyverse)
source("./code/functions/process_helpers.R")

text_files <- list.files(path = "./data/raw",pattern = ".*.txt")

all_documents <- c()
for(i in 1:length(text_files)){
  documents <- extract_full_text_id(paste0("./data/raw/", text_files[i]))
  all_documents <- c(all_documents, documents)
}

save(all_documents, file = "./data/all_documents.Rda")

