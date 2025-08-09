source("00_libraries.R")

# Used in file '02_processing.Rmd'
extract_first_sentence <- function(text) {
  if (is.na(text) || text == "") {
    return(NA_character_)
  }
  
  first_sentence <- str_extract(text, "^.*?[.!?](?=\\s|$)")
  
  if (is.na(first_sentence)) {
    return(trimws(text))
  } else {
    return(trimws(first_sentence))
  }
}

# Used in file '03_evaluating.Rmd'
get_subtree_ids <- function(tokens, head_id) {
  ids <- c(head_id)
  repeat {
    new_ids <- tokens$token_id[tokens$head_token_id %in% ids & !(tokens$token_id %in% ids)]
    if (length(new_ids) == 0) break
    ids <- c(ids, new_ids)
  }
  ids
}
