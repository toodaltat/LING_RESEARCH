library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(udpipe)
library(qualtRics)
library(knitr)
library(readr)
library(tidyverse)

R.version.string

data_path <- ("C:\\Users\\Ethan\\OneDrive\\Cloud_Desktop\\00_UC\\00_2025_S2\\00_LING\\00_analysis\\data_sets")
model_path <- "C:\\Users\\Ethan\\OneDrive\\Cloud_Desktop\\00_UC\\00_2025_S2\\00_LING\\00_analysis\\news_analysis\\english-ewt-ud-2.5-191206.udpipe"

################################################################################
# Loading model
################################################################################

udmodel <- udpipe_load_model(file = model_path)

################################################################################
# Sentence extraction
################################################################################

# Used in file '02_processing.Rmd'
extract_first_sentence <- function(texts) {
  texts[texts == ""] <- NA_character_
  
  sentence_boundary <- "(?<!\\b(?:[A-Z]\\.){1,5}|\\b(?:No|Fig|Mr|Mrs|Dr)\\.)[.!?](?:\\s|$)"
  
  split_matrix <- str_split(texts, sentence_boundary, n = 2, simplify = TRUE)
  
  first_sentences <- trimws(split_matrix[, 1])
  return(first_sentences)
}
