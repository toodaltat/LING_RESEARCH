source("00_libraries.R")

################################################################################
# Main script
################################################################################

raw <- load_raw_data(file.path(data_path, "LING 310 - 74465159 Survey_October 1, 2025_09.30.csv"))

questions <- extract_questions(raw)

df_long <- reshape_data(raw, questions)

df_long <- clean_data(df_long, group_lookup)
