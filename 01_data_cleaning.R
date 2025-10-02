source("00_libraries.R")

################################################################################
# Load and clean CSVs
################################################################################

df_fake <- read.csv(file.path(data_path, "fake.csv"))
df_true <- read.csv(file.path(data_path, "true.csv"))

# Remove 'Reuters' from true texts
df_true$text <- str_replace(df_true$text, "^.*?(?:\\(Reuters\\)\\s*-\\s*)", "")

df_true <- head(df_true, 500)
df_fake <- head(df_fake, 500)

################################################################################
# Combine, extract first sentences, filter for 3+ commas
################################################################################

df_sentences <- bind_rows(
  df_true %>% mutate(truth_value = TRUE),
  df_fake %>% mutate(truth_value = FALSE)
) %>%
  mutate(
    first_sentence = extract_first_sentence(text),
    comma_count = str_count(first_sentence, ","),
    period_count = str_count(first_sentence, "\\."),
    entry = row_number()
  ) %>%
  filter(comma_count >= 3) %>%
  filter(period_count == 0) %>% 
  select(entry, first_sentence, truth_value, comma_count)

rownames(df_sentences) <- NULL
