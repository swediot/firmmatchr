#' @importFrom stats quantile
#' @importFrom dplyr %>%
NULL

utils::globalVariables(c(
    "name_clean", "N", "query_id", "match_type", "dict_id", "raw_name", "score",
    ".row_id_internal", "block", "fts_q", "jw_sim", "token", "token_weight", "tokens",
    "name_clean.x", "name_clean.y", "LLM_decision", "rarity"
))
