library(tidyverse)
library(udpipe)
library(qualtRics)
library(knitr)
library(ggplot2)

R.version.string

data_path <- ("C:\\Users\\Ethan\\OneDrive\\Cloud_Desktop\\00_UC\\00_2025_S2\\00_LING\\00_analysis\\data_sets")
model_path <- "C:\\Users\\Ethan\\OneDrive\\Cloud_Desktop\\00_UC\\00_2025_S2\\00_LING\\00_analysis\\news_analysis\\english-ewt-ud-2.5-191206.udpipe"

################################################################################
# Loading model
################################################################################

udmodel <- udpipe_load_model(file = model_path)

################################################################################
# Function for sentence extraction
################################################################################

# Used in file '02_processing.Rmd'
extract_first_sentence <- function(texts) {
  texts[texts == ""] <- NA_character_
  
  sentence_boundary <- "(?<!\\b(?:[A-Z]\\.){1,5}|\\b(?:No|Fig|Mr|Mrs|Dr)\\.)[.!?](?:\\s|$)"
  
  split_matrix <- str_split(texts, sentence_boundary, n = 2, simplify = TRUE)
  
  first_sentences <- trimws(split_matrix[, 1])
  return(first_sentences)
}

################################################################################
# Functions for survey handling
################################################################################

# Defining questions category
groupings <- list(
  TT = c("Q4", "Q9", "Q15", "Q17"),
  TF = c("Q1", "Q5", "Q8", "Q14"),
  FT = c("Q2", "Q6", "Q11", "Q12", "Q13"),
  FF = c("Q3", "Q7", "Q10", "Q16", "Q18")
)

group_lookup <- map2(groupings, names(groupings),
                     ~ setNames(rep(.y, length(.x)), .x)) %>%
  flatten_chr()

# Reading in CSV
load_raw_data <- function(path) {
  readr::read_csv(path, col_names = FALSE,
                  show_col_types = FALSE)
}

# Seperates questions into a look up dataframe
extract_questions <- function(raw) {
  names_short <- unlist(raw[1, ])
  names_long  <- unlist(raw[2, ])
  
  tibble(
    question_code = names_short[grep("^Q[0-9]", names_short)],
    question_text = names_long[grep("^Q[0-9]", names_short)]
  ) %>%
    mutate(across(everything(), as.character))
}

# Handles missing responses
extract_missing_responses <- function(df_long) {
  df_long %>%
    filter(is.na(response) | response == "" | question_code == "Q0")
}


# Moves from wide to long
reshape_data <- function(raw, questions) {
  # Removes header rows and assigns column names
  names_short <- unlist(raw[1, ])
  df <- raw[-c(1, 2), ]
  names(df) <- names_short
  # Reshapes to long  
  df %>%
    pivot_longer(
      cols = starts_with("Q"),
      names_to = "question_code",
      values_to = "response"
    ) %>%
    left_join(questions, by = "question_code") %>%
    select(ResponseId, StartDate, EndDate, Age, Origin,
           question_code, response) %>%
    rename(
      response_id = ResponseId,
      start_date  = StartDate,
      end_date    = EndDate,
      age         = Age,
      origin      = Origin
    )
}


# Cleans and handles missing entries
clean_data <- function(df_long, group_lookup) {
  df_long %>%
    slice(-(1:19)) %>%
    mutate(
      question_code = str_remove(question_code, "_1$"),
      # Applies question grouping
      question_code = if_else(
        question_code %in% names(group_lookup),
        paste0(question_code, "_", group_lookup[question_code]),
        question_code
      )
    ) %>%
    filter(!is.na(response), response != "", question_code != "Q0") %>%
    mutate(
      response = as.numeric(response),
      across(where(is.character), str_trim),
      origin = case_when(
        str_to_lower(origin) == "nz" ~ "New Zealand",
        TRUE ~ origin
      ),
      # Categorizing response
      category = case_when(
        response <= 3 ~ "low",
        response <= 6 ~ "mid",
        response >= 7 ~ "high",
        TRUE ~ NA_character_
      ),
      # Extracting last char as mod_factor
      mod_factor = str_sub(question_code, -1)
    )
}