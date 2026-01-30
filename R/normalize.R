#' Normalize Company Names
#'
#' Standardizes company names by lowercasing, removing legal suffixes,
#' translating characters to ASCII, and removing noise words.
#'
#' @param x A character vector of company names.
#' @return A character vector of normalized names.
#' @importFrom stringi stri_trans_general
#' @export
normalize_company_name <- function(x) {
  if (length(x) == 0) return(character(0))
  
  # Lowercase and trim
  x <- tolower(x)
  x <- trimws(x)
  
  # Standardize separators
  x <- gsub("[&/\\+]", " ", x)
  
  # German-specific substitutions (pre-transliteration)
  x <- gsub("\u00df", "ss", x) # Eszett
  x <- gsub("\u00e4", "ae", x) # ä
  x <- gsub("\u00f6", "oe", x) # ö
  x <- gsub("\u00fc", "ue", x) # ü
  
  # Remove legal forms (Canonization)
  # Words must be standalone (\b boundaries)
  suffixes <- c(
    "gmbh", "ag", "kg", "ohg", "ug", "se", "kgaa", "limited",
    "sa", "sarl", "sas", "spa", "srl", "ltd", "inc", "llc", "plc",
    "co", "corp", "holding", "group", "gruppe", "einzelfirma",
    "genossenschaft", "stiftung", "verein"
  )
  
  suffix_pattern <- paste0("\\b(", paste(suffixes, collapse = "|"), ")\\b")
  x <- gsub(suffix_pattern, " ", x)
  
  # Remove "noise" words
  noise <- c(
    "und", "et", "cie", "filiale", "partner", 
    "deutschland", "germany", "international", "standort", "geschlossen"
  )
  noise_pattern <- paste0("\\b(", paste(noise, collapse = "|"), ")\\b")
  x <- gsub(noise_pattern, " ", x)
  
  # Transliterate (accents to ASCII)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  
  # Remove punctuation and non-alphanumeric (keep digits and spaces)
  x <- gsub("[^a-z0-9 ]", " ", x)
  
  # Collapse whitespace
  x <- gsub("\\s+", " ", x)
  
  trimws(x)
}