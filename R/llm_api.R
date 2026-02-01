#' @importFrom httr POST add_headers timeout status_code content
#' @importFrom jsonlite toJSON fromJSON
#' @import glue
#' @import cli
NULL

#' Internal Azure Chat Completion Wrapper (Custom Endpoint)
#'
#' Sends a request to a custom Azure-like endpoint (e.g. /openai/v1/responses).
#'
#' @param system_msg String. The instructions for the LLM.
#' @param user_msg String. The specific case to evaluate.
#' @param endpoint String. Base URL.
#' @param api_key String. API Key.
#' @param deployment String. Model/Deployment name.
#' @param api_version String. API version (unused in this custom path but kept for compatibility).
#' @return A character string (the JSON response) or NULL on failure.
#' @keywords internal
azure_chat_request <- function(system_msg,
                               user_msg,
                               endpoint,
                               api_key,
                               deployment,
                               api_version = "2024-04-14") {
  # 1. Construct URL (Restoring your original logic)
  base <- sub("/+$", "", endpoint)
  url <- paste0(base, "/openai/v1/responses")

  # 2. Construct Body (Restoring your original 'input' structure)
  # Your original script used 'input' list with roles, and 'model' in body.
  body <- list(
    model = deployment,
    input = list(
      list(role = "system", content = system_msg),
      list(role = "user", content = user_msg)
    ),
    temperature = 0,
    max_output_tokens = 256
  )

  json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)

  # 3. Retry Logic
  max_retries <- 3

  for (i in 1:max_retries) {
    resp <- tryCatch(
      {
        httr::POST(
          url,
          httr::add_headers(
            `Content-Type` = "application/json",
            `api-key`      = api_key
          ),
          body = json_body,
          encode = "json",
          httr::timeout(60)
        )
      },
      error = function(e) NULL
    )

    if (!is.null(resp)) {
      status <- httr::status_code(resp)
      if (status == 200) break

      if (status >= 500 || status == 429) {
        Sys.sleep(2 * i)
      } else {
        # Fatal error (like 404) - print it to help debugging
        cli::cli_alert_danger("API Fatal Error: {status}")
        return(NULL)
      }
    } else {
      Sys.sleep(1)
    }
  }

  if (is.null(resp) || httr::status_code(resp) != 200) {
    return(NULL)
  }

  # 4. Parse Response (Restoring your specific parsing logic)
  # Your endpoint returns a different structure than standard Azure
  parsed <- tryCatch(
    {
      content <- httr::content(resp, "text", encoding = "UTF-8")
      jsonlite::fromJSON(content, simplifyVector = FALSE)
    },
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(NULL)
  }

  # Try to extract text based on your original script's logic
  # Logic 1: output_text field
  if (!is.null(parsed$output_text)) {
    if (is.character(parsed$output_text)) {
      return(paste(parsed$output_text, collapse = "\n"))
    }
  }

  # Logic 2: output[[1]]$content[[1]]$text
  if (!is.null(parsed$output) && length(parsed$output) > 0) {
    first <- parsed$output[[1]]
    if (!is.null(first$content) && length(first$content) > 0) {
      if (!is.null(first$content[[1]]$text)) {
        return(first$content[[1]]$text)
      }
    }
  }

  # Fallback: return raw content if we can't parse the specific field
  return(NULL)
}
