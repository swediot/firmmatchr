#' @import data.table
#' @import zoomerjoin
#' @import stringdist
#' @import DBI
#' @import RSQLite
#' @import stringi
NULL

# ==============================================================================
# Engine 1: Exact Match
# ==============================================================================
engine_exact <- function(queries, dictionary) {
  matches <- merge(
    queries[, list(query_id, name_clean)],
    dictionary[, list(dict_id, name_clean)],
    by = "name_clean",
    all = FALSE
  )

  if (nrow(matches) > 0) matches[, match_type := "Perfect"]

  unique(matches[, list(query_id, dict_id, match_type)])
}

# ==============================================================================
# Engine 2: Fuzzy (Zoomerjoin)
# ==============================================================================
engine_fuzzy_zoomer <- function(queries, dictionary, threshold_zoomer = 0.4, threshold_jw = 0.8) {

  queries[, block := substr(name_clean, 1, 1)]
  dictionary[, block := substr(name_clean, 1, 1)]

  q_df <- as.data.frame(queries[, list(query_id, name_clean, block)])
  d_df <- as.data.frame(dictionary[, list(dict_id, name_clean, block)])

  res <- suppressMessages(
    zoomerjoin::jaccard_inner_join(
      q_df, d_df,
      by = "name_clean",
      block_by = "block",
      n_gram_width = 3,
      threshold = threshold_zoomer,
      n_bands = 500
    )
  )

  setDT(res)

  if (nrow(res) == 0) return(data.table(query_id = character(), dict_id = character(), match_type = character()))

  res[, jw_sim := stringdist::stringsim(name_clean.x, name_clean.y, method = "jw")]
  res <- res[jw_sim >= threshold_jw]

  setorder(res, query_id, -jw_sim)
  best <- res[, .SD[1], by = query_id]

  best[, list(query_id, dict_id, match_type = "Fuzzy (Zoomer)")]
}

# ==============================================================================
# Engine 3: FTS5 (SQLite)
# ==============================================================================
engine_fts <- function(queries, dictionary, threshold_jw = 0.8) {

  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  dbExecute(con, "CREATE VIRTUAL TABLE dict_fts USING fts5(dict_id, name_clean)")
  dbWriteTable(con, "dict_fts", dictionary[, list(dict_id, name_clean)], append = TRUE)

  make_query <- function(s) {
    if (!nzchar(s)) return(NA_character_)
    parts <- strsplit(s, "\\s+")[[1]]
    if (length(parts) == 0) return(NA_character_)
    paste0(parts, "*", collapse = " OR ")
  }

  queries[, fts_q := sapply(name_clean, make_query)]
  queries_to_run <- queries[!is.na(fts_q)]

  results <- list()

  for (i in seq_len(nrow(queries_to_run))) {
    qid <- queries_to_run$query_id[i]
    qnm <- queries_to_run$name_clean[i]
    q_str <- queries_to_run$fts_q[i]

    safe_q <- gsub("'", "''", q_str)

    sql <- sprintf(
      "SELECT dict_id, name_clean FROM dict_fts WHERE dict_fts MATCH '%s' LIMIT 25",
      safe_q
    )

    cands <- tryCatch(dbGetQuery(con, sql), error = function(e) NULL)

    if (!is.null(cands) && nrow(cands) > 0) {
      sims <- stringdist::stringsim(qnm, cands$name_clean, method = "jw")
      max_idx <- which.max(sims)

      if (sims[max_idx] >= threshold_jw) {
        results[[i]] <- data.table(
          query_id = qid,
          dict_id = cands$dict_id[max_idx],
          match_type = "Fuzzy (FTS)"
        )
      }
    }
  }

  rbindlist(results)
}

# ==============================================================================
# Engine 4: Rarity Weighted (MatchMakeR Logic)
# ==============================================================================
engine_rarity <- function(queries, dictionary, threshold_rarity = 1.0) {

  # 1. Tokenize (split by non-alphanumeric, keep len >= 5)
  tokenize <- function(txt) {
    tokens <- stringi::stri_split_regex(txt, "[^a-z0-9]+")
    lapply(tokens, function(x) x[nchar(x) >= 5])
  }

  # Copy to avoid side effects
  q_dt <- copy(queries)
  d_dt <- copy(dictionary)

  q_dt[, tokens := tokenize(name_clean)]
  d_dt[, tokens := tokenize(name_clean)]

  # 2. Build Dictionary Index & Calc Global Rarity
  dict_long <- d_dt[, list(token = unlist(tokens)), by = dict_id]
  token_counts <- dict_long[, .N, by = token]

  # Remove common words (Top 20% frequent)
  if (nrow(token_counts) > 0) {
    cutoff <- quantile(token_counts$N, 0.8)
    token_counts <- token_counts[N <= cutoff]
  }
  token_counts[, rarity := 1 / N]

  # 3. Calculate Query Token Weights (Normalization Step)
  # This makes the sum of weights for a query = 1.0
  q_long <- q_dt[, list(token = unlist(tokens)), by = query_id]

  # Attach rarity to query tokens
  q_weighted <- merge(q_long, token_counts, by = "token", sort = FALSE)

  # Calculate normalized weight: How important is this token *for this query*?
  q_weighted[, token_weight := rarity / sum(rarity), by = query_id]

  # 4. Match against Dictionary
  # Join query tokens to dictionary tokens
  matches_long <- merge(q_weighted, dict_long, by = "token", allow.cartesian = TRUE, sort = FALSE)

  # 5. Score
  # Sum the normalized weights.
  # If a query consists of 2 rare words and we match both, score = 1.0.
  scores <- matches_long[, list(score = sum(token_weight)), by = list(query_id, dict_id)]

  # Filter
  scores <- scores[score >= threshold_rarity]

  if (nrow(scores) == 0) return(data.table(query_id = character(), dict_id = character(), match_type = character()))

  setorder(scores, query_id, -score)
  best <- scores[, .SD[1], by = query_id]

  best[, list(query_id, dict_id, match_type = "Rarity (Token)")]
}
