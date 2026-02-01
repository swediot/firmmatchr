#' Validate Matches using LLM (Azure OpenAI)
#'
#' Sends doubtful matches (not "Perfect" or "Unmatched") to an LLM for verification.
#' Supports resuming from interruptions via chunk files.
#'
#' @param data Data frame. Must contain the columns specified by `query_name_col` and `dict_name_col`.
#' @param query_name_col String. Column containing the user's query name (Employer).
#' @param dict_name_col String. Column containing the dictionary match name (Registry).
#' @param output_dir String. Directory to save temporary chunks and final results.
#' @param filename_stem String. Base name for output files.
#' @param batch_size Integer. Number of rows to process before saving a chunk.
#' @param api_key String. Azure API Key. Defaults to `Sys.getenv("AZURE_API_KEY")`.
#' @param endpoint String. Azure Endpoint. Defaults to `Sys.getenv("AZURE_ENDPOINT")`.
#' @param deployment String. Deployment name. Defaults to `Sys.getenv("AZURE_DEPLOYMENT")`.
#' @return A data frame with added `LLM_decision` and `LLM_reason` columns.
#' @importFrom dplyr mutate filter select left_join case_when as_tibble row_number tibble
#' @import readr
#' @importFrom purrr pmap_dfr map_dfr
#' @import glue
#' @import cli
#' @export
validate_matches_llm <- function(data,
                                 query_name_col,
                                 dict_name_col,
                                 output_dir = "llm_checkpoints",
                                 filename_stem = "match_validation",
                                 batch_size = 20,
                                 api_key = Sys.getenv("AZURE_API_KEY"),
                                 endpoint = Sys.getenv("AZURE_ENDPOINT"),
                                 deployment = Sys.getenv("AZURE_DEPLOYMENT")) {
  # 1. Validation
  if (api_key == "" || endpoint == "") {
    cli::cli_abort("Azure credentials missing. Please set AZURE_API_KEY and AZURE_ENDPOINT.")
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Check if columns exist
  if (!query_name_col %in% names(data)) cli::cli_abort("Column {.val {query_name_col}} not found in data.")
  if (!dict_name_col %in% names(data)) cli::cli_abort("Column {.val {dict_name_col}} not found in data.")

  # 2. Filter Rows to Check
  # Create a standardized internal tibble
  df_in <- as_tibble(data) %>%
    mutate(.row_id_internal = row_number())

  to_check <- df_in %>%
    filter(
      !is.na(dict_id), # Must be matched
      match_type != "Perfect", # Skip Perfect matches
      match_type != "Manual" # Skip Manual matches
    )

  if (nrow(to_check) == 0) {
    cli::cli_alert_success("No doubtful matches found. Returning original data.")
    return(data)
  }

  cli::cli_h1("Starting LLM Validation")
  cli::cli_alert_info("Total rows to validate: {nrow(to_check)}")
  cli::cli_alert_info("Checkpoints: {.path {output_dir}}")

  # 3. System Prompt
  sys_msg <- paste(
    "You are an expert on German company registries.",
    "Your task: decide if the match between an Employer Name (from reviews) and an Official Registry Name is CORRECT.",
    "Definitions:",
    "- CORRECT: Same legal entity, subsidiary, or clear corporate group match.",
    "- INCORRECT: Different company, no clear relation, or unsure.",
    "Clarifications:",
    "- Ignore legal suffixes (GmbH, AG) unless they distinguish totally different firms.",
    "- Allow small typos/abbreviations.",
    "Output strict JSON: {\"decision\": \"CORRECT\" (or \"INCORRECT\"), \"reason\": \"short explanation\"}"
  )

  # 4. Processing Loop
  chunks <- split(to_check, ceiling(seq_len(nrow(to_check)) / batch_size))
  total_chunks <- length(chunks)

  pb <- cli::cli_progress_bar("Processing Chunks", total = total_chunks)

  for (i in seq_along(chunks)) {
    chunk_file <- file.path(output_dir, sprintf("%s_chunk_%04d.csv", filename_stem, i))

    if (file.exists(chunk_file)) {
      cli::cli_progress_update(id = pb)
      next
    }

    batch <- chunks[[i]]

    # Use standard extraction [[ ]] to avoid scope issues
    q_names <- batch[[query_name_col]]
    d_names <- batch[[dict_name_col]]

    results <- purrr::pmap_dfr(list(q_names, d_names, batch$.row_id_internal), function(q, d, rid) {
      user_msg <- glue::glue("Employer: {q}\nRegistry Entry: {d}\n\nIs this the same company?")

      resp_json <- azure_chat_request(sys_msg, user_msg, endpoint, api_key, deployment)

      decision <- "ERROR"
      reason <- "API Failed"

      if (!is.null(resp_json)) {
        parsed <- tryCatch(jsonlite::fromJSON(resp_json), error = function(e) NULL)
        if (!is.null(parsed)) {
          decision <- toupper(parsed$decision)
          reason <- parsed$reason
        }
      }

      tibble(
        .row_id_internal = rid,
        LLM_decision = decision,
        LLM_reason = reason
      )
    })

    readr::write_csv(results, chunk_file)
    cli::cli_progress_update(id = pb)
  }

  # 5. Merge Results
  cli::cli_alert_info("Merging chunks...")
  chunk_files <- list.files(output_dir, pattern = sprintf("^%s_chunk_\\d+.csv$", filename_stem), full.names = TRUE)

  if (length(chunk_files) == 0) {
    return(data)
  }

  llm_results <- purrr::map_dfr(chunk_files, readr::read_csv, show_col_types = FALSE)

  final_df <- df_in %>%
    left_join(llm_results, by = ".row_id_internal") %>%
    mutate(
      LLM_incorrect = case_when(
        LLM_decision == "INCORRECT" ~ 1L,
        LLM_decision == "CORRECT" ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    select(-.row_id_internal)

  return(final_df)
}
