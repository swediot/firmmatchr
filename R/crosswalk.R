#' Dictionary Crosswalk of a Match Result
#'
#' Because [normalize_company_name()] is lossy, several dictionary rows can
#' collapse onto the same normalized string (e.g. `"Meier GmbH"` and
#' `"Meier AG"` both become `"meier"`). [match_companies()] matches against one
#' representative row per group and attaches this crosswalk so that every
#' original dictionary row can still be recovered.
#'
#' @param x A result returned by [match_companies()].
#' @return A `data.table` with one row per original dictionary row that entered
#'   matching, and columns `name_clean` (the normalized string),
#'   `dict_id`/`dict_name` (the original id and string), `dict_id_rep` (the id
#'   reported by [match_companies()] for that group), `n_dict_ids` (group size)
#'   and `is_representative`.
#' @seealso [expand_matches()]
#' @export
#' @examples
#' queries <- data.frame(
#'   query_id = 1:2,
#'   company_name = c("Meier", "BMW")
#' )
#' dictionary <- data.frame(
#'   orbis_id = c("D001", "D002", "D003"),
#'   company_name = c("Meier GmbH", "Meier AG", "BMW AG")
#' )
#'
#' \donttest{
#' results <- match_companies(queries, dictionary)
#'
#' # Both Meier rows survive in the crosswalk, even though only D001 is
#' # reported as the match.
#' dict_crosswalk(results)
#' }
dict_crosswalk <- function(x) {
  cw <- attr(x, "dict_crosswalk", exact = TRUE)
  if (is.null(cw)) {
    cli::cli_abort(c(
      "x" = "No dictionary crosswalk found on {.arg x}.",
      "i" = "Pass the object returned by {.fun match_companies}; subsetting it drops the attribute."
    ))
  }
  data.table::copy(cw)
}

#' Expand Matches to Every Original Dictionary Row
#'
#' [match_companies()] reports one representative dictionary id per match. When
#' normalization collapsed several dictionary rows onto the same string, this
#' function expands the result to one row per (query row, original dictionary
#' row) pair, so the original strings behind a collapsed group are visible.
#'
#' @param x A result returned by [match_companies()].
#' @param crosswalk The dictionary crosswalk. Defaults to the one attached to
#'   `x` by [match_companies()]; supply it explicitly if `x` has been subset or
#'   round-tripped through another format.
#' @return A `data.table` with columns `query_id`, `query_name`, `dict_id`,
#'   `dict_name`, `match_type`, `dict_id_rep`, `n_dict_ids` and
#'   `is_representative`. Unambiguous matches contribute exactly one row.
#' @seealso [dict_crosswalk()]
#' @export
#' @examples
#' queries <- data.frame(
#'   query_id = 1:2,
#'   company_name = c("Meier", "BMW")
#' )
#' dictionary <- data.frame(
#'   orbis_id = c("D001", "D002", "D003"),
#'   company_name = c("Meier GmbH", "Meier AG", "BMW AG")
#' )
#'
#' \donttest{
#' results <- match_companies(queries, dictionary)
#'
#' # One row for BMW, two rows for the collapsed Meier group.
#' expand_matches(results)
#' }
expand_matches <- function(x, crosswalk = dict_crosswalk(x)) {
  res <- data.table::as.data.table(x)

  required <- c("query_id", "query_name", "dict_id", "match_type")
  missing <- setdiff(required, names(res))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg x} is missing column{?s} {.val {missing}}.")
  }

  cw <- data.table::as.data.table(crosswalk)
  cw_required <- c("dict_id", "dict_name", "dict_id_rep", "n_dict_ids", "is_representative")
  cw_missing <- setdiff(cw_required, names(cw))
  if (length(cw_missing) > 0) {
    cli::cli_abort("{.arg crosswalk} is missing column{?s} {.val {cw_missing}}.")
  }

  out <- merge(
    res[, list(query_id, query_name, dict_id_rep = dict_id, match_type)],
    cw[, list(dict_id_rep, dict_id, dict_name, n_dict_ids, is_representative)],
    by = "dict_id_rep",
    allow.cartesian = TRUE
  )

  data.table::setorder(out, query_id, -is_representative, dict_id)
  out[, list(
    query_id, query_name, dict_id, dict_name, match_type,
    dict_id_rep, n_dict_ids, is_representative
  )]
}
