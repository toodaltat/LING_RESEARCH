source("01_tools.R")

################################################################################
# Loading csv
################################################################################

df_fake <- read.csv(file.path(file_path, "fake.csv"))

df_true <- read.csv(file.path(file_path, "true.csv"))

################################################################################
# Cleaning #
################################################################################

# Removing 'Reuters' to avoid giving a non-linguistic hint
df_true$text <- stringr::str_replace(df_true$text, "^.*?(?:\\(Reuters\\)\\s*-\\s*)", "")

# Cheap way to over come 'U.S.' interrupting sentence 
df_true$text <- gsub("U.S.", "United States", df_true$text, fixed = TRUE)

################################################################################
# Extraction
################################################################################

# Building true news dataframe
df_sentence_true <- data.frame(
  entry = 1:nrow(df_true),
  first_sentence = sapply(df_true$text, extract_first_sentence),
  truth_value = TRUE
)
rownames(df_sentence_true) <- NULL
df_sentence_true <- head(df_sentence_true, 500)

# Building fake news dataframe
df_sentence_fake <- data.frame(
  entry = 1:nrow(df_fake),
  first_sentence = sapply(df_fake$text, extract_first_sentence),
  truth_value = FALSE
)
rownames(df_sentence_fake) <- NULL
df_sentence_fake <- head(df_sentence_fake, 500)

################################################################################
# Collecting the first 100 sentences and filtering by sentences that have more than 100 characters
################################################################################

entries <- (1:100)

fake_df <- df_sentence_fake %>%
  filter(entry %in% entries, nchar(first_sentence) > 100)

true_df <- df_sentence_true %>%
  filter(entry %in% entries, nchar(first_sentence) > 100)

