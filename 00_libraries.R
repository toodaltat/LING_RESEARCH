library(tidyverse)
library(udpipe)
library(qualtRics)
library(knitr)
library(ggplot2)
library(kableExtra)
library(gridExtra)
library(grid)
library(ragg)

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

# Marking branching status
branching_lookup <- tribble(
  ~question, ~branching,
  "Q1", "left",
  "Q2", "right",
  "Q3", "left",
  "Q4", "right",
  "Q5", "left",
  "Q6", "right",
  "Q7", "left",
  "Q8", "left",
  "Q9", "right",
  "Q10", "left",
  "Q11", "left",
  "Q12", "right",
  "Q13", "right",
  "Q14", "left",
  "Q15", "right",
  "Q16", "left",
  "Q17", "right",
  "Q18", "right"
)


# Standard
standardize_question_code <- function(df, group_lookup) {
  df %>%
    mutate(
      # remove trailing _1 if present
      question_code = str_remove(question_code, "_1$"),
      # append group suffix (_TF, _FT, etc.) if exists in lookup
      question_code = if_else(
        question_code %in% names(group_lookup),
        paste0(question_code, "_", group_lookup[question_code]),
        question_code
      )
    )
}

# Reading in CSV
load_raw_data <- function(path) {
  readr::read_csv(path, col_names = FALSE,
                  show_col_types = FALSE)
}


# Separates questions into a look up dataframe
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


# Grabbing unique items
unique_items <- function(df) {
  df %>% distinct(response_id, .keep_all = TRUE)
}


# Allows for time spent caculations
calculate_time_spent <- function(df, start_col = "start_date", end_col = "end_date") {
  df <- df %>%
    mutate(
      !!start_col := as.POSIXct(.data[[start_col]], format = "%Y-%m-%d %H:%M:%S"),
      !!end_col   := as.POSIXct(.data[[end_col]], format = "%Y-%m-%d %H:%M:%S"),
      time_spent_mins = as.numeric(difftime(.data[[end_col]], .data[[start_col]], units = "mins"))
    )
  
  return(df)
}

# Cleans missing dataframe
clean_missing_responses <- function(df) {
  df <- df %>%
    mutate(
      response = as.numeric(response),
      across(where(is.character), str_trim),
      origin = case_when(
        str_to_lower(origin) %in% c("nz", "new zealand", "aotearoa", "aotearoa new zealand") ~ "New Zealand",
        str_to_lower(origin) == "thai" ~ "Thailand",
        TRUE ~ origin
      )
    )
  
  return(df)
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
        str_to_lower(origin) == "england" ~ "United Kingdom",
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
      mod_factor = str_sub(question_code, -1),
      truth_factor = str_sub(str_extract(question_code, "(?<=_)[:alpha:]{2}"), 1, 1),
      question_clean = sub("_.*", "", question_code)
    ) %>%
    # Join branching lookup
    left_join(branching_lookup, by = c("question_clean" = "question")) %>% 
    select(-question_clean)
}


################################################################################
# Function for CRD rating
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
  
  # Immediate Constituents (IC)
  ic_counts <- anno %>%
    group_by(head_token_id) %>%
    summarise(IC = n(), .groups = "drop")
  
  total_ic <- sum(ic_counts$IC)
  
  # Constituent Recognition Domain (CRD)
  crd <- anno %>%
    mutate(token_id = as.numeric(token_id)) %>% 
    group_by(head_token_id) %>%
    summarise(
      CRD = max(token_id, na.rm = TRUE) - min(token_id, na.rm = TRUE) + 1,
      .groups = "drop"
    )
  
  avg_crd <- mean(crd$CRD, na.rm = TRUE)  # average CRD per constituent
  
  # IC to word ratio
  total_words <- nrow(anno)
  ic_to_word_ratio <- total_ic / total_words
  
  return(list(CRD = avg_crd, IC_to_word_ratio = ic_to_word_ratio))
}

# Vectorized wrapper for a dataframe column
analyze_texts <- function(texts, udmodel, codes = NULL) {
  results <- lapply(texts, analyze_syntax, udmodel = udmodel)
  results_df <- bind_rows(lapply(results, as.data.frame))
  
  if(!is.null(codes)) {
    results_df <- results_df %>% 
      mutate(question_code = codes) %>% 
      select(question_code, everything())
  }
  
  return(results_df)
  
}


# Function to join CRD ratings and drop IC_to_word_ratio
join_crd <- function(df, crd_ratings) {
  df %>%
    left_join(crd_ratings, by = "question_code") %>%
    select(-IC_to_word_ratio)
}


################################################################################
# Functions for table creation
################################################################################

# Function to save a table as PNG
save_table_png <- function(df, filename, width = 800, height = 600, res = 150) {
  # Convert data frame to table grob
  table_grob <- tableGrob(df, rows = NULL)
  
  # Open PNG device
  agg_png(filename, width = width, height = height, res = res)
  
  # Draw the table
  grid.draw(table_grob)
  
  # Close device
  dev.off()
  
  message(paste("Table saved as", filename))
}


# Origin
summarise_origin_counts <- function(df_valid, df_missing, group_col = "origin") {
  
  counts_valid <- df_valid %>%
    group_by(.data[[group_col]]) %>%
    summarise(count = n(), .groups = "drop")
  
  counts_missing <- df_missing %>%
    group_by(.data[[group_col]]) %>%
    summarise(count = n(), .groups = "drop")
  
  summary <- full_join(counts_valid, counts_missing,
                       by = group_col,
                       suffix = c("_valid", "_missing")) %>%
    replace_na(list(count_valid = 0, count_missing = 0)) %>%
    mutate(total_responses = count_valid + count_missing) %>%
    bind_rows(
      tibble(
        !!group_col := "TOTAL",
        count_valid = sum(.$count_valid),
        count_missing = sum(.$count_missing),
        total_responses = sum(.$total_responses)
      )
      ) %>%
    rename(
      Country = .data[[group_col]],
      "Valid Responses" = count_valid,
      "Invalid Responses" = count_missing,
      "Total Responses" = total_responses
    )
  
  return(summary)
}

create_summary_table <- function(df, response_col = "response", time_col = "time_spent_mins", id_col = "response_id") {
  
  summary_table <- df %>%
    summarise(
      total_responses = n(),
      unique_participants = n_distinct(.data[[id_col]]),
      avg_response = mean(.data[[response_col]], na.rm = TRUE),
      sd_response = sd(.data[[response_col]], na.rm = TRUE),
      avg_time_spent = mean(.data[[time_col]], na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Factor",
      values_to = "Value"
    ) %>%
    mutate(
      Factor = recode(Factor,
                      total_responses = "Total Responses",
                      unique_participants = "Unique Participants",
                      avg_response = "Average Response",
                      sd_response = "Std of Response",
                      avg_time_spent = "Average Time (mins)"
      ),
      Value = case_when(
        Factor %in% c("Total Responses", "Unique Participants") ~ as.character(round(Value, 0)),
        TRUE ~ as.character(round(Value, 2))
      )
    )
  
  return(summary_table)
}

