source("00_libraries.R")

################################################################################
# Sentence extraction
################################################################################

# Used in file '02_processing.Rmd'
abbreviations <- c(
  "Mr\\.", "Mrs\\.", "Ms\\.", "Dr\\.", "Prof\\.", "Jr\\.", "Sr\\.",
  "vs\\.", "etc\\.", "e\\.g\\.", "i\\.e\\.", "U\\.S\\.", "U\\.K\\.", "U\\.S\\.A\\.", "Ph\\.D\\."
)
abbr_pattern <- paste0("(?<!", paste(abbreviations, collapse = "|"), ")")
sentence_boundary <- paste0("(?<=", abbr_pattern, "[.!?])\\s+")

# Vectorized version
extract_first_sentence <- function(texts) {
  # Handle NA or empty strings up front
  texts[texts == ""] <- NA_character_
  
  # Split all texts at once (returns a matrix if simplify=TRUE)
  split_matrix <- stringr::str_split(texts, sentence_boundary, simplify = TRUE)
  
  # Take the first column (first sentence), trim whitespace
  first_sentences <- trimws(split_matrix[, 1])
  
  return(first_sentences)
}


################################################################################
# Framework for clause extraction
################################################################################

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


################################################################################
# CRD // IC to word ratio
################################################################################

# Function to calculate CRD + IC-to-word ratio for one text
analyze_syntax <- function(text, udmodel) {
  if (is.na(text) || text == "") {
    return(list(CRD = NA, IC_to_word_ratio = NA))
  }
  
  # Parse text
  anno <- udpipe_annotate(udmodel, x = text)
  anno <- as.data.frame(anno)
  
  if (nrow(anno) == 0) {
    return(list(CRD = NA, IC_to_word_ratio = NA))
  }
  
  # --- Immediate Constituents (IC) ---
  ic_counts <- anno %>%
    group_by(head_token_id) %>%
    summarise(IC = n(), .groups = "drop")
  
  total_ic <- sum(ic_counts$IC)
  
  # --- CRD (Constituent Recognition Domain) ---
  crd <- anno %>%
    mutate(token_id = as.numeric(token_id)) %>% 
    group_by(head_token_id) %>%
    summarise(
      CRD = max(token_id, na.rm = TRUE) - min(token_id, na.rm = TRUE) + 1,
      .groups = "drop"
    )
  
  avg_crd <- mean(crd$CRD, na.rm = TRUE)  # average CRD per constituent
  
  # --- IC-to-word ratio ---
  total_words <- nrow(anno)
  ic_to_word_ratio <- total_ic / total_words
  
  return(list(CRD = avg_crd, IC_to_word_ratio = ic_to_word_ratio))
}

# Vectorized wrapper for a dataframe column
analyze_texts <- function(texts, udmodel) {
  results <- lapply(texts, analyze_syntax, udmodel = udmodel)
  results_df <- bind_rows(lapply(results, as.data.frame))
  return(results_df)
}
