source("00_libraries.R")

################################################################################
# Main script
################################################################################

raw <- load_raw_data(file.path(data_path, "LING 310 - 74465159 Survey_October 14, 2025_11.10.csv"))

questions <- extract_questions(raw)

questions <- standardize_question_code(questions, group_lookup)

crd_ratings <- analyze_texts(questions$question_text, udmodel, codes = questions$question_code)

df_raw <- reshape_data(raw, questions)

################################################################################

df <- clean_data(df_raw, group_lookup)

df <- calculate_time_spent(df)

df <- join_crd(df, crd_ratings)

df$category <- factor(df$category, levels = c("low", "mid", "high"))

################################################################################

df_missing <- extract_missing_responses(df_raw)

df_missing <- clean_missing_responses(df_missing)

df_missing_unique <- unique_items(df_missing)

################################################################################

df_unique <- unique_items(df)

################################################################################
# Main script part 2
################################################################################

df_native <- df %>% 
  filter(origin %in% c("New Zealand", "United Kingdom"))

################################################################################

missing_summary <- df_missing_unique %>%
  filter(is.na(response) | response == "") %>%
  summarise(total_missing = n())

################################################################################

df_origin_time_summary <- df %>%
  group_by(response_id, origin) %>%
  summarise(avg_time_spent = mean(time_spent_mins, na.rm = TRUE))

################################################################################
