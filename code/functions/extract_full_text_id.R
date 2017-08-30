## functions to help with processing raw documents data

extract_full_text_id <- function(document_text_file # the name of a text file containing documents pulled from ProQuest
                                 ){
  
  tx <- read_lines(document_text_file)
  start_of_text <- grep("Full text: ", tx)
  sep_lines <- grep("^____________________________________________________________$", tx) # every document is separated by these underscores
  
  documents <- list()
  for (i in 1:length(start_of_text)){
    bookends <- tx[sep_lines[i]:sep_lines[i+1]]
    has_ID <- length(grep("ProQuest document ID:", bookends))>0
    
    # get full text
    initial_pull <- tx[start_of_text[i]:(sep_lines[i+1]-1)]
    blank_lines <- which(!is.na(match(initial_pull, "")))
    if(length(blank_lines)>2){
      start_of_meta <-blank_lines[which(diff(blank_lines)==2)[1]]
    }
    else{
      start_of_meta <- blank_lines[1]
    }
    # note: assuming the start of the meta data starts after the first blank line ("") and where the next blank line is 2 lines away is imperfect 
    # alot of the documents have credits, photo info etc at the end
    # it will do for now. 
    final_pull <- initial_pull[1:(start_of_meta-1)]
    
    if(has_ID){
      # get unique ID
      storeID_text <- bookends[grep("ProQuest document ID:", bookends)]
      storeID <- as.numeric(gsub(pattern = "ProQuest document ID: ","", storeID_text))
    }
    if(!has_ID){
      storeID_text <- bookends[grep("https://search.proquest.com/docview/", bookends)][1]
      storeID <- as.numeric(gsub(pattern = "https://search.proquest.com/docview/|(\\?.*)","", storeID_text))
    }
    documents[[i]] <- list(text = final_pull, storeID = storeID)
  }
  
  return(documents)
}