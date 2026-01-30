#' Match Company Names against a Dictionary
#'
#' Runs a cascading matching pipeline: Exact -> Fuzzy (Zoomer) -> FTS5 -> Rarity.
#' Matches found in earlier steps are removed from subsequent steps.
#'
#' @param queries Data frame. Must contain columns specified in `query_col` and `unique_id_col`.
#' @param dictionary Data frame. Must contain columns specified in `dict_col` and `dict_id_col`.
#' @param query_col String. Column name for company names in `queries`.
#' @param dict_col String. Column name for company names in `dictionary`.
#' @param unique_id_col String. ID column in `queries`.
#' @param dict_id_col String. ID column in `dictionary`.
#' @param threshold_jw Numeric (0-1). Minimum Jaro-Winkler similarity. Default 0.8.
#' @param threshold_zoomer Numeric (0-1). Jaccard threshold for blocking. Default 0.4.
#' @param threshold_rarity Numeric. Minimum score for rarity matching. Default 1.0.
#' @param n_cores Integer. Number of cores (reserved for future parallel implementation).
#' @return A data.table containing `query_id`, `dict_id`, and `match_type`.
#' @import data.table
#' @import cli
#' @import progressr
#' @export
match_companies <- function(queries,
                            dictionary,
                            query_col = "company_name",
                            dict_col = "company_name",
                            unique_id_col = "query_id",
                            dict_id_col = "orbis_id",
                            threshold_jw = 0.8,
                            threshold_zoomer = 0.4,
                            threshold_rarity = 1.0,
                            n_cores = 1) {

  # Header
  cli::cli_h1("Starting Company Matching Pipeline")
  cli::cli_alert_info("Params: JW={threshold_jw} | Zoomer={threshold_zoomer} | Rarity={threshold_rarity}")

  # 1. Setup Data
  # Convert to data.table (shallow copy)
  q_raw <- as.data.table(queries)
  d_raw <- as.data.table(dictionary)

  # Check if columns exist using robust base R checks
  if (!query_col %in% names(q_raw)) cli::cli_abort("Column {.val {query_col}} not found in queries.")
  if (!unique_id_col %in% names(q_raw)) cli::cli_abort("Column {.val {unique_id_col}} not found in queries.")
  if (!dict_col %in% names(d_raw)) cli::cli_abort("Column {.val {dict_col}} not found in dictionary.")
  if (!dict_id_col %in% names(d_raw)) cli::cli_abort("Column {.val {dict_id_col}} not found in dictionary.")

  # --- Direct Extraction to avoid scope issues ---
  q_dt <- data.table(
    query_id = as.character(q_raw[[unique_id_col]]),
    raw_name = q_raw[[query_col]]
  )

  d_dt <- data.table(
    dict_id = as.character(d_raw[[dict_id_col]]),
    raw_name = d_raw[[dict_col]]
  )

  # 2. Validation & Cleaning
  cli::cli_alert_info("Normalizing strings...")
  d_dt[, name_clean := normalize_company_name(raw_name)]

  # STRICT DUPLICATE CHECK
  dups <- d_dt[, .N, by = name_clean][N > 1]
  if (nrow(dups) > 0) {
    cli::cli_abort(c(
      "x" = "Dictionary contains {nrow(dups)} duplicate normalized names.",
      "i" = "Example duplicate: '{dups$name_clean[1]}'",
      "i" = "Please deduplicate your dictionary (e.g. keep highest revenue) before running."
    ))
  }

  q_dt[, name_clean := normalize_company_name(raw_name)]

  # 3. Execution Pipeline
  all_matches <- list()
  remaining <- q_dt

  progressr::with_progress({
    p <- progressr::progressor(steps = 4)

    # --- Step 1: Exact ---
    p(message = "Step 1/4: Exact Matching")
    m1 <- engine_exact(remaining, d_dt)
    if (nrow(m1) > 0) {
      all_matches[[1]] <- m1
      remaining <- remaining[!query_id %in% m1$query_id]
    }
    cli::cli_alert_success("Exact Match: Found {nrow(m1)} matches.")

    # --- Step 2: Fuzzy (Zoomer) ---
    if (nrow(remaining) > 0) {
      p(message = "Step 2/4: Fuzzy (Zoomer)")
      m2 <- engine_fuzzy_zoomer(remaining, d_dt, threshold_zoomer, threshold_jw)
      if (nrow(m2) > 0) {
        all_matches[[2]] <- m2
        remaining <- remaining[!query_id %in% m2$query_id]
      }
      cli::cli_alert_success("Fuzzy Zoomer: Found {nrow(m2)} matches.")
    } else {
      p(message = "Skipping Fuzzy (all matched)")
    }

    # --- Step 3: FTS5 ---
    if (nrow(remaining) > 0) {
      p(message = "Step 3/4: FTS5 Search")
      m3 <- engine_fts(remaining, d_dt, threshold_jw)
      if (nrow(m3) > 0) {
        all_matches[[3]] <- m3
        remaining <- remaining[!query_id %in% m3$query_id]
      }
      cli::cli_alert_success("FTS5 Match: Found {nrow(m3)} matches.")
    } else {
      p(message = "Skipping FTS5 (all matched)")
    }

    # --- Step 4: Rarity ---
    if (nrow(remaining) > 0) {
      p(message = "Step 4/4: Rarity Token Search")
      m4 <- engine_rarity(remaining, d_dt, threshold_rarity)
      if (nrow(m4) > 0) {
        all_matches[[4]] <- m4
        remaining <- remaining[!query_id %in% m4$query_id]
      }
      cli::cli_alert_success("Rarity Match: Found {nrow(m4)} matches.")
    } else {
      p(message = "Skipping Rarity (all matched)")
    }
  })

  # 4. Final Compilation
  final_results <- rbindlist(all_matches)

  cli::cli_h1("Pipeline Complete")
  if (nrow(final_results) > 0) {
    cli::cli_text("Matches by type:")

    # --- FIX: Format as readable list strings ---
    summary_dt <- final_results[, .N, by = match_type]
    # Create strings like "Exact Match: 129"
    formatted_list <- paste0(summary_dt$match_type, ": ", summary_dt$N)

    # Output cleanly
    cli::cli_ul(formatted_list)

  } else {
    cli::cli_alert_warning("No matches found.")
  }

  return(final_results)
}
