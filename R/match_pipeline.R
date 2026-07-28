# ==============================================================================
# Internal helpers: name preparation, de-duplication and crosswalk building
# ==============================================================================

# Empty, correctly typed match table. Used so that every engine and every
# early return has the same schema (avoids "column not found" downstream).
.fm_empty_matches <- function() {
  data.table::data.table(
    query_id = character(0),
    dict_id = character(0),
    match_type = character(0)
  )
}

# Build a two-column table (original id, original string) plus the normalized
# string that the matching engines actually operate on.
.fm_prepare_side <- function(ids, names, normalize, lang, extra_stopwords) {
  dt <- data.table::data.table(
    in_id = as.character(ids),
    in_name = as.character(names)
  )
  dt[, name_clean := if (isTRUE(normalize)) {
    normalize_company_name(in_name, lang = lang, extra_stopwords = extra_stopwords)
  } else {
    trimws(in_name)
  }]
  dt[]
}

# Rows whose normalized name is NA or empty cannot be matched on: an empty
# string would "exactly match" every other empty string. Drop them loudly.
.fm_drop_unusable <- function(dt, side) {
  bad <- is.na(dt$name_clean) | !nzchar(dt$name_clean)
  if (any(bad)) {
    cli::cli_alert_warning(
      "Dropping {sum(bad)} {side} row{?s} whose name is empty or missing after normalization."
    )
    dt <- dt[!bad]
  }
  dt
}

# Collapse a prepared dictionary onto unique normalized names.
#
# Returns a list with
#   index     : one row per unique normalized name (what the engines see),
#               carrying the representative id/name and the full id list.
#   crosswalk : one row per surviving *original* dictionary row, mapping it to
#               the representative id. This is what guarantees that collapsed
#               duplicates can always be traced back to their source rows.
.fm_collapse_dictionary <- function(d_dt, on_duplicate) {
  d_dt <- data.table::copy(d_dt)
  d_dt[, n_dict_ids := .N, by = name_clean]

  n_dup_groups <- data.table::uniqueN(d_dt[n_dict_ids > 1L]$name_clean)

  if (n_dup_groups > 0L && identical(on_duplicate, "error")) {
    example <- d_dt[n_dict_ids > 1L]$name_clean[1L]
    cli::cli_abort(c(
      "x" = "Dictionary contains {n_dup_groups} duplicate normalized name{?s}.",
      "i" = "Example duplicate: {.val {example}}",
      "i" = "Deduplicate the dictionary first, or use {.code on_duplicate = \"collapse\"}."
    ))
  }

  if (n_dup_groups > 0L) {
    n_dup_rows <- nrow(d_dt[n_dict_ids > 1L])
    cli::cli_alert_warning(c(
      "Normalization collapsed {n_dup_rows} dictionary row{?s} into ",
      "{n_dup_groups} distinct name{?s}."
    ))
    cli::cli_alert_info(
      "The first row of each group is used as the representative; see {.fun dict_crosswalk} / {.fun expand_matches} to recover all original rows."
    )
  }

  # `by=` keeps first-appearance order, so [1L] is the first original row.
  d_dt[, `:=`(
    dict_id_rep = in_id[1L],
    dict_name_rep = in_name[1L],
    dict_id_all = paste(in_id, collapse = "|")
  ), by = name_clean]

  index <- unique(
    d_dt[, list(
      name_clean,
      dict_id = dict_id_rep,
      dict_name = dict_name_rep,
      n_dict_ids,
      dict_id_all
    )],
    by = "name_clean"
  )

  d_dt[, is_representative := seq_len(.N) == 1L, by = name_clean]

  crosswalk <- d_dt[, list(
    name_clean,
    dict_id = in_id,
    dict_name = in_name,
    dict_id_rep,
    n_dict_ids,
    is_representative
  )]

  list(index = index, crosswalk = crosswalk[])
}

#' Match Company Names against a Dictionary
#'
#' Runs a cascading matching pipeline: Exact -> Fuzzy (Zoomer) -> FTS5 -> Rarity.
#' Matches found in earlier steps are removed from subsequent steps. Names are
#' normalized with German, French, Italian and English conventions by default;
#' see the `lang` argument.
#'
#' @section Duplicate handling:
#' Normalization is lossy on purpose: `"Meier GmbH"` and `"Meier AG"` both
#' become `"meier"`. Distinct dictionary rows can therefore collapse onto the
#' same normalized string. Rather than failing, `match_companies()` groups such
#' rows, matches against one representative per group, and keeps a full
#' crosswalk so no original row is ever lost:
#'
#' * `dict_id` in the result is the representative (first) id of the group.
#' * `dict_id_all` lists every id in the group, separated by `"|"`.
#' * `n_dict_ids` is the size of the group (`1` when there was no collapse).
#' * [dict_crosswalk()] returns the mapping from every original dictionary row
#'   to its representative, and [expand_matches()] expands a result table to one
#'   row per original dictionary row.
#'
#' Set `on_duplicate = "error"` to restore the older, strict behaviour of
#' aborting when duplicates are present.
#'
#' Queries are handled the same way: identical normalized query names are
#' matched once and the result is fanned back out, so duplicated query names
#' (and even duplicated query ids) are safe.
#'
#' Rows whose name is empty or `NA` after normalization are dropped with a
#' warning, since an empty string would otherwise match every other empty string.
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
#' @param dict_norm Logical. Apply [normalize_company_name()] to the dictionary
#'   names? Set to `FALSE` if the dictionary is already cleaned. Default `TRUE`.
#' @param query_norm Logical. Apply [normalize_company_name()] to the query
#'   names? Set to `FALSE` if the queries are already cleaned. Default `TRUE`.
#'   Both sides should normally use the same setting.
#' @param lang Character vector of language conventions to apply when
#'   normalizing. Any of `"de"` (German), `"fr"` (French), `"it"` (Italian),
#'   `"en"` (English). Defaults to all four, which suits mixed-language
#'   registers such as the Swiss one. Narrow it for single-language data to
#'   strip fewer tokens. See [normalize_company_name()].
#' @param extra_stopwords Optional character vector of additional whole-word
#'   tokens to strip from both sides, on top of the built-in lists. Country
#'   names are kept by default; pass them here to remove them.
#' @param on_duplicate String. What to do when normalization maps several
#'   dictionary rows onto the same string: `"collapse"` (default) groups them
#'   and keeps a crosswalk, `"error"` aborts.
#' @return A `data.table` with one row per matched query row and columns
#'   `query_id`, `query_name`, `dict_id`, `dict_name`, `match_type`,
#'   `n_dict_ids` and `dict_id_all`. The dictionary crosswalk is attached as the
#'   `"dict_crosswalk"` attribute; retrieve it with [dict_crosswalk()].
#' @seealso [dict_crosswalk()], [expand_matches()]
#' @import data.table
#' @import cli
#' @import progressr
#' @export
#' @examples
#' # Wrapped in \donttest{} because the pipeline calls the multi-threaded Rust
#' # internals of 'zoomerjoin'.
#' \donttest{
#' # Create sample query data
#' queries <- data.frame(
#'   query_id = 1:3,
#'   company_name = c("BMW", "Siemens AG", "Deutsche Bank")
#' )
#'
#' # Dictionary with two rows that collapse onto the same normalized name
#' # ("siemens"): the pipeline groups them instead of failing.
#' dictionary <- data.frame(
#'   orbis_id = c("D001", "D002", "D003", "D004"),
#'   company_name = c("BMW AG", "Siemens AG", "Siemens GmbH", "Commerzbank AG")
#' )
#'
#' results <- match_companies(
#'   queries = queries,
#'   dictionary = dictionary,
#'   query_col = "company_name",
#'   dict_col = "company_name",
#'   unique_id_col = "query_id",
#'   dict_id_col = "orbis_id"
#' )
#'
#' print(results)
#'
#' # Recover every original dictionary row behind a collapsed match
#' print(expand_matches(results))
#' }
match_companies <- function(queries,
                            dictionary,
                            query_col = "company_name",
                            dict_col = "company_name",
                            unique_id_col = "query_id",
                            dict_id_col = "orbis_id",
                            threshold_jw = 0.8,
                            threshold_zoomer = 0.4,
                            threshold_rarity = 1.0,
                            n_cores = 1,
                            dict_norm = TRUE,
                            query_norm = TRUE,
                            lang = c("de", "fr", "it", "en"),
                            extra_stopwords = NULL,
                            on_duplicate = c("collapse", "error")) {
  on_duplicate <- match.arg(on_duplicate)
  lang <- match.arg(lang, several.ok = TRUE)

  # Header
  cli::cli_h1("Starting Company Matching Pipeline")
  cli::cli_alert_info("Params: JW={threshold_jw} | Zoomer={threshold_zoomer} | Rarity={threshold_rarity}")

  # 1. Setup Data
  q_raw <- data.table::as.data.table(queries)
  d_raw <- data.table::as.data.table(dictionary)

  if (!query_col %in% names(q_raw)) cli::cli_abort("Column {.val {query_col}} not found in queries.")
  if (!unique_id_col %in% names(q_raw)) cli::cli_abort("Column {.val {unique_id_col}} not found in queries.")
  if (!dict_col %in% names(d_raw)) cli::cli_abort("Column {.val {dict_col}} not found in dictionary.")
  if (!dict_id_col %in% names(d_raw)) cli::cli_abort("Column {.val {dict_id_col}} not found in dictionary.")

  # 2. Normalization (optional per side)
  if (isTRUE(dict_norm) || isTRUE(query_norm)) {
    cli::cli_alert_info("Normalization languages: {.val {lang}}.")
  }
  cli::cli_alert_info(
    "Dictionary names: {if (isTRUE(dict_norm)) 'normalizing' else 'used as supplied'}."
  )
  cli::cli_alert_info(
    "Query names: {if (isTRUE(query_norm)) 'normalizing' else 'used as supplied'}."
  )

  d_dt <- .fm_prepare_side(
    d_raw[[dict_id_col]], d_raw[[dict_col]], dict_norm, lang, extra_stopwords
  )
  q_dt <- .fm_prepare_side(
    q_raw[[unique_id_col]], q_raw[[query_col]], query_norm, lang, extra_stopwords
  )

  d_dt <- .fm_drop_unusable(d_dt, "dictionary")
  q_dt <- .fm_drop_unusable(q_dt, "query")

  if (nrow(d_dt) == 0L) cli::cli_abort("No usable dictionary rows left after normalization.")

  # 3. Collapse duplicates on both sides, keeping a full crosswalk
  dict_parts <- .fm_collapse_dictionary(d_dt, on_duplicate)
  d_idx <- dict_parts$index
  crosswalk <- dict_parts$crosswalk

  q_dt[, qorder := .I]
  q_idx <- unique(q_dt[, list(name_clean)], by = "name_clean")
  q_idx[, query_id := paste0("..q", .I)] # internal key, mapped back at the end

  n_q_collapsed <- nrow(q_dt) - nrow(q_idx)
  if (n_q_collapsed > 0L) {
    cli::cli_alert_info(
      "{n_q_collapsed} duplicate query name{?s} collapsed; each distinct name is matched once."
    )
  }

  # 4. Execution Pipeline
  all_matches <- list()
  remaining <- q_idx

  progressr::with_progress({
    p <- progressr::progressor(steps = 4)

    # --- Step 1: Exact ---
    p(message = "Step 1/4: Exact Matching")
    m1 <- engine_exact(remaining, d_idx)
    if (nrow(m1) > 0) {
      all_matches[[1]] <- m1
      remaining <- remaining[!query_id %in% m1$query_id]
    }
    cli::cli_alert_success("Exact Match: Found {nrow(m1)} matches.")

    # --- Step 2: Fuzzy (Zoomer) ---
    if (nrow(remaining) > 0) {
      p(message = "Step 2/4: Fuzzy (Zoomer)")
      m2 <- engine_fuzzy_zoomer(remaining, d_idx, threshold_zoomer, threshold_jw)
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
      m3 <- engine_fts(remaining, d_idx, threshold_jw)
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
      m4 <- engine_rarity(remaining, d_idx, threshold_rarity)
      if (nrow(m4) > 0) {
        all_matches[[4]] <- m4
        remaining <- remaining[!query_id %in% m4$query_id]
      }
      cli::cli_alert_success("Rarity Match: Found {nrow(m4)} matches.")
    } else {
      p(message = "Skipping Rarity (all matched)")
    }
  })

  matched <- if (length(all_matches) > 0L) {
    data.table::rbindlist(all_matches, use.names = TRUE)
  } else {
    .fm_empty_matches()
  }

  # 5. Map internal keys back to the user's original rows
  final_results <- .fm_assemble(matched, q_dt, q_idx, d_idx)
  data.table::setattr(final_results, "dict_crosswalk", crosswalk)

  cli::cli_h1("Pipeline Complete")
  if (nrow(final_results) > 0) {
    cli::cli_text("Matches by type:")
    summary_dt <- final_results[, .N, by = match_type]
    cli::cli_ul(paste0(summary_dt$match_type, ": ", summary_dt$N))

    n_ambiguous <- nrow(final_results[n_dict_ids > 1L])
    if (n_ambiguous > 0) {
      cli::cli_alert_warning(
        "{n_ambiguous} match{?es} point{?s/} to a collapsed dictionary group; see {.code dict_id_all}."
      )
    }
  } else {
    cli::cli_alert_warning("No matches found.")
  }

  final_results[]
}

# Fan internal per-normalized-name matches back out to the original query rows
# and attach the dictionary side.
.fm_assemble <- function(matched, q_dt, q_idx, d_idx) {
  empty <- data.table::data.table(
    query_id = character(0),
    query_name = character(0),
    dict_id = character(0),
    dict_name = character(0),
    match_type = character(0),
    n_dict_ids = integer(0),
    dict_id_all = character(0)
  )
  if (nrow(matched) == 0L) return(empty)

  keyed <- merge(
    matched[, list(query_id, dict_id, match_type)],
    q_idx[, list(query_id, name_clean)],
    by = "query_id"
  )

  out <- merge(
    q_dt[, list(query_id = in_id, query_name = in_name, name_clean, qorder)],
    keyed[, list(name_clean, dict_id, match_type)],
    by = "name_clean"
  )
  if (nrow(out) == 0L) return(empty)

  out <- merge(
    out,
    d_idx[, list(dict_id, dict_name, n_dict_ids, dict_id_all)],
    by = "dict_id"
  )

  data.table::setorder(out, qorder)
  out[, list(query_id, query_name, dict_id, dict_name, match_type, n_dict_ids, dict_id_all)]
}
