source("02_processing.R")

################################################################################
# Loading model
################################################################################

true_df <- sentence_true
fake_df <- sentence_fake

udmodel <- udpipe_download_model(language = "english-ewt")
udmodel <- udpipe_load_model(file = udmodel$file_model)


################################################################################
# Loading model with content
################################################################################
annotations <- udpipe_annotate(udmodel, x = true_df$first_sentence, doc_id = true_df$entry)
annotations <- udpipe_annotate(udmodel, x = fake_df$first_sentence, doc_id = fake_df$entry)

true_annotations_df <- as.data.frame(annotations)
fake_annotations_df <- as.data.frame(annotations)


################################################################################
# Noun phrases
################################################################################
true_noun_chunks <- keywords_phrases(
  x = true_annotations_df$upos,
  term = true_annotations_df$token,
  pattern = "(DET )?(ADJ )*NOUN",
  is_regex = TRUE,
  detailed = TRUE
)

fake_noun_chunks <- keywords_phrases(
  x = fake_annotations_df$upos,
  term = fake_annotations_df$token,
  pattern = "(DET )?(ADJ )*NOUN",
  is_regex = TRUE,
  detailed = TRUE
)

################################################################################
# Adverbial clauses
################################################################################

# True adverbial clauses
adv_clauses_true <- true_annotations_df %>%
  group_by(doc_id) %>%
  group_modify(~{
    advcl_tokens <- .x %>% dplyr::filter(dep_rel == "advcl")
    if (nrow(advcl_tokens) == 0) {
      return(tibble(type = character(), clause = character()))
    }
    clause_list <- lapply(advcl_tokens$token_id, function(id) {
      ids <- get_subtree_ids(.x, id)
      clause_tokens <- .x %>% dplyr::filter(token_id %in% ids)
      paste(clause_tokens$token, collapse = " ")
    })
    tibble(type = "Adverbial Clause", clause = unlist(clause_list))
  })

# Fake adverbial clauses
adv_clauses_fake <- fake_annotations_df %>%
  group_by(doc_id) %>%
  group_modify(~{
    advcl_tokens <- .x %>% dplyr::filter(dep_rel == "advcl")
    if (nrow(advcl_tokens) == 0) {
      return(tibble(type = character(), clause = character()))
    }
    clause_list <- lapply(advcl_tokens$token_id, function(id) {
      ids <- get_subtree_ids(.x, id)
      clause_tokens <- .x %>% dplyr::filter(token_id %in% ids)
      paste(clause_tokens$token, collapse = " ")
    })
    tibble(type = "Adverbial Clause", clause = unlist(clause_list))
  })
################################################################################