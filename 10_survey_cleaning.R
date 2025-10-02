source("00_libraries.R")

################################################################################
# Loading survey results
################################################################################

raw <- read.csv(
  file.path(data_path, "LING 310 - 74465159 Survey_October 1, 2025_09.30.csv"),
  header = FALSE,
  stringsAsFactors = FALSE
)

################################################################################
# Extract names and prepare look up table
################################################################################

names_short <- unlist(raw[1, ])
names_long  <- unlist(raw[2, ])

df <- raw[-c(1,2), ]
names(df) <- names_short

question_cols <- grep("^Q[0-9]", names(df))

questions <- tibble(
  question_code = names_short[question_cols],
  question_text = names_long[question_cols]
) %>%
  mutate(across(everything(), as.character))

################################################################################
# Reshaping into long format
################################################################################

df_long <- df %>%
  pivot_longer(
    cols = all_of(question_cols),
    names_to = "question_code",
    values_to = "response"
  ) %>%
  left_join(questions, by = "question_code") %>%
  select(
    ResponseId, StartDate, EndDate,
    Age, Origin,
    question_code, response
  ) %>%
  rename(
    response_id = ResponseId,
    start_date  = StartDate,
    end_date    = EndDate,
    age         = Age,
    origin      = Origin
  )

################################################################################
# Cleaning
################################################################################

# Define question groupings
TT <- c("Q4", "Q9", "Q15", "Q17")
TF <- c("Q1", "Q5", "Q8", "Q14")
FT <- c("Q2", "Q6", "Q11", "Q12", "Q13")
FF <- c("Q3", "Q7", "Q10", "Q16", "Q18")

group_lookup <- c(
  setNames(rep("TT", length(TT)), TT),
  setNames(rep("TF", length(TF)), TF),
  setNames(rep("FT", length(FT)), FT),
  setNames(rep("FF", length(FF)), FF)
)

df_long <- df_long %>%
  slice(-(1:19)) %>%
  
  mutate(
    question_code = gsub("_1$", "", question_code),
    question_code = ifelse(
      question_code %in% names(group_lookup),
      paste0(question_code, "_", group_lookup[question_code]),
      question_code
    )
  ) %>%
  
  filter(!is.na(response) & response != "" & question_code != "Q0") %>%
  mutate(response = as.numeric(response)) %>%
  
  mutate(
    across(where(is.character), str_trim),
    origin = case_when(
      str_to_lower(origin) == "nz" ~ "New Zealand",
      TRUE ~ origin
    )
  )

df_long <- df_long %>%
  mutate(category = case_when(
    response <= 3 ~ "low",
    response > 3 & response <= 6 ~ "mid",
    response >= 7 ~ "high",
    TRUE ~ NA_character_
  ))

df_long$mod_factor <- substr(df_long$question_code,
                             nchar(df_long$question_code),
                             nchar(df_long$question_code))

# --- join back if needed ---
# full_data <- responses %>%
#   left_join(questions, by = "question_code")
