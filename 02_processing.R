# Loading libraries, checking versions and availability 
source("01_tools.R")

################################################################################
# Loading csv
################################################################################
news_fake <- read.csv(file.path(file_path, "fake.csv"))

news_true <- read.csv(file.path(file_path, "true.csv"))
################################################################################


################################################################################
# Cleaning #
################################################################################

# Removing 'Reuters' to avoid giving a non-linguistic hint
news_true$text <- stringr::str_replace(news_true$text, "^.*?(?:\\(Reuters\\)\\s*-\\s*)", "")

# Cheap way to over come 'U.S.' interrupting sentence 
news_true$text <- gsub("U.S.", "United States", news_true$text, fixed = TRUE)
################################################################################


################################################################################
# Extraction
################################################################################

# Building true news dataframe
one_sentence_true <- data.frame(
  entry = 1:nrow(news_true),
  first_sentence = sapply(news_true$text, extract_first_sentence),
  truth_value = TRUE
)
rownames(one_sentence_true) <- NULL
one_sentence_true <- head(one_sentence_true, 500)

# Building fake news dataframe
one_sentence_fake <- data.frame(
  entry = 1:nrow(news_fake),
  first_sentence = sapply(news_fake$text, extract_first_sentence),
  truth_value = FALSE
)
################################################################################


################################################################################
# Collecting the first 100 sentences and filtering by sentences that have more than 100 characters
################################################################################

entries <- (1:100)

sentence_fake <- one_sentence_fake %>%
  filter(entry %in% entries, nchar(first_sentence) > 100)

sentence_true <- one_sentence_true %>%
  filter(entry %in% entries, nchar(first_sentence) > 100)