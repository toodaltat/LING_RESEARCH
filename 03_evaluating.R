source("02_processing.R")

################################################################################
# Loading model
################################################################################

# true_df <- sentence_true
# fake_df <- sentence_fake

udmodel <- udpipe_download_model(language = "english-ewt")
udmodel <- udpipe_load_model(file = udmodel$file_model)


################################################################################
# Loading model with content
################################################################################
annotations_true <- udpipe_annotate(udmodel, x = true_df$first_sentence, doc_id = true_df$entry)
annotations_fake <- udpipe_annotate(udmodel, x = fake_df$first_sentence, doc_id = fake_df$entry)

true_annotations_df <- as.data.frame(annotations_true)
fake_annotations_df <- as.data.frame(annotations_fake)


################################################################################
# Relative Clauses
################################################################################

# True relative clauses
rel_clauses_true <- true_annotations_df %>%
  group_by(doc_id) %>%
  group_modify(~{
    relcl_tokens <- .x %>% dplyr::filter(dep_rel == "acl:relcl")
    if (nrow(relcl_tokens) == 0) {
      return(tibble(type = character(), clause = character()))
    }
    clause_list <- lapply(relcl_tokens$token_id, function(id) {
      ids <- get_subtree_ids(.x, id)
      clause_tokens <- .x %>% dplyr::filter(token_id %in% ids)
      paste(clause_tokens$token, collapse = " ")
    })
    tibble(type = "Relative Clause", clause = unlist(clause_list))
  })

# Fake relative clauses
rel_clauses_fake <- fake_annotations_df %>%
  group_by(doc_id) %>%
  group_modify(~{
    relcl_tokens <- .x %>% dplyr::filter(dep_rel == "acl:relcl")
    if (nrow(relcl_tokens) == 0) {
      return(tibble(type = character(), clause = character()))
    }
    clause_list <- lapply(relcl_tokens$token_id, function(id) {
      ids <- get_subtree_ids(.x, id)
      clause_tokens <- .x %>% dplyr::filter(token_id %in% ids)
      paste(clause_tokens$token, collapse = " ")
    })
    tibble(type = "Relative Clause", clause = unlist(clause_list))
  })

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
# Table join
################################################################################

# True table
joined_df_true <- rel_clauses_true %>%
  inner_join(adv_clauses_true, by = "doc_id", suffix = c("_rel", "_adv"))

final_true_sentence_df <- true_annotations_df %>%
  inner_join(joined_df_true, by = "doc_id") %>%
  distinct(doc_id, .keep_all = TRUE) %>% 
  select(-paragraph_id, -sentence_id)


# Fake table
joined_df_fake <- rel_clauses_fake %>%
  inner_join(adv_clauses_fake, by = "doc_id", suffix = c("_rel", "_adv"))

final_fake_sentence_df <- fake_annotations_df %>%
  inner_join(joined_df_fake, by = "doc_id") %>%
  distinct(doc_id, .keep_all = TRUE) %>% 
  select(-paragraph_id, -sentence_id)

