source("00_libraries.R")

################################################################################
# Main script
################################################################################

raw <- load_raw_data(file.path(data_path, "LING 310 - 74465159 Survey_October 7, 2025_11.56.csv"))

questions <- extract_questions(raw)

df_raw <- reshape_data(raw, questions)

df_missing <-extract_missing_responses(df_raw)

df <- clean_data(df_raw, group_lookup)

