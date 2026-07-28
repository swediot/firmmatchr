#' @importFrom stats quantile
#' @importFrom dplyr %>%
NULL

utils::globalVariables(c(
    "name_clean", "N", "query_id", "match_type", "dict_id", "raw_name", "score",
    ".row_id_internal", "block", "fts_q", "jw_sim", "token", "token_weight", "tokens",
    "name_clean.x", "name_clean.y", "LLM_decision", "rarity",
    "in_id", "in_name", "qorder", "query_name", "dict_name",
    "dict_id_rep", "dict_name_rep", "dict_id_all", "n_dict_ids", "is_representative"
))
