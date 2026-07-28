# ==============================================================================
# Language rule tables
#
# Legal forms are corporate-form tokens that carry no identifying information
# ("gmbh", "sarl", "spa", "ltd"). Noise words are geographic / structural
# filler ("deutschland", "succursale", "and").
#
# Kept deliberately conservative: only tokens that are meaningless on their own.
# Words that can identify a firm are NOT listed -- "deutsche" stays, so
# "Deutsche Bank" does not collapse to "bank".
# ==============================================================================

.fm_legal_forms <- list(
  de = c(
    "gmbh", "mbh", "ag", "kg", "kgaa", "ohg", "ug", "gbr", "eg", "ev", "se",
    "gesellschaft", "aktiengesellschaft", "genossenschaft", "stiftung",
    "verein", "einzelfirma", "einzelunternehmen", "gruppe", "holding"
  ),
  fr = c(
    "sa", "sarl", "sas", "sasu", "sci", "scs", "sca", "snc", "eurl", "scop",
    "scic", "sem", "societe", "ste", "cie", "association", "fondation",
    "cooperative", "groupe", "entreprise", "etablissement", "etablissements",
    "ets"
  ),
  it = c(
    "spa", "srl", "srls", "sapa", "sas", "snc", "scarl", "scrl", "societa",
    "ditta", "impresa", "gruppo", "fondazione", "associazione", "cooperativa"
  ),
  en = c(
    "ltd", "limited", "inc", "incorporated", "llc", "llp", "plc", "corp",
    "corporation", "company", "co", "holding", "holdings", "group"
  )
)

# Note on geography: country names are deliberately NOT stripped. They are
# identity as often as they are noise -- removing them turns "Credit Suisse"
# into "credit" and "Air France" into "air", which can then exact-match an
# unrelated firm. Since a spurious exact match is trusted and never reaches the
# fuzzy stages or the LLM validator, the asymmetry favours keeping them. The
# structural branch markers below ("niederlassung", "succursale") are the part
# that is genuinely noise. Add country names back via `extra_stopwords` if your
# data needs it.
.fm_noise_words <- list(
  de = c(
    "und", "standort", "filiale", "niederlassung", "zweigniederlassung",
    "geschlossen"
  ),
  fr = c(
    "et", "succursale", "filiale",
    "la", "le", "les", "du", "des", "de", "aux", "au"
  ),
  it = c(
    "e", "di", "del", "dello", "della", "dei", "degli", "delle",
    "la", "il", "lo", "gli", "filiale", "succursale"
  ),
  en = c(
    "and", "the", "of", "international", "branch", "subsidiary"
  )
)

#' Stop Words Used by the Normalizer
#'
#' Returns the legal-form and noise-word tokens that
#' [normalize_company_name()] strips for a given set of languages. Useful for
#' auditing what the normalizer removes before trusting a match.
#'
#' @param lang Character vector of language codes. Any of `"de"` (German),
#'   `"fr"` (French), `"it"` (Italian), `"en"` (English). Defaults to all four.
#' @return A `data.frame` with columns `lang`, `type` (`"legal_form"` or
#'   `"noise"`) and `token`.
#' @seealso [normalize_company_name()]
#' @export
#' @examples
#' # Everything stripped for Italian
#' subset(company_name_stopwords("it"), type == "legal_form")
#'
#' # How many tokens are removed in total across all four languages?
#' nrow(company_name_stopwords())
company_name_stopwords <- function(lang = c("de", "fr", "it", "en")) {
  lang <- match.arg(lang, several.ok = TRUE)

  build <- function(tbl, type) {
    do.call(rbind, lapply(lang, function(l) {
      data.frame(
        lang = l, type = type, token = tbl[[l]],
        stringsAsFactors = FALSE
      )
    }))
  }

  rbind(
    build(.fm_legal_forms, "legal_form"),
    build(.fm_noise_words, "noise")
  )
}

# Union of the selected languages' tokens, longest first so that alternation
# never matches a short prefix of a longer token.
.fm_stopword_pattern <- function(tbl, lang) {
  words <- unique(unlist(tbl[lang], use.names = FALSE))
  words <- words[order(-nchar(words), words)]
  paste0("\\b(", paste(words, collapse = "|"), ")\\b")
}

#' Normalize Company Names
#'
#' Standardizes company names by lowercasing, transliterating to ASCII,
#' removing legal forms and noise words, and collapsing punctuation. Supports
#' German, French, Italian and English conventions, which covers the four
#' languages of Swiss and neighbouring company registers.
#'
#' @details
#' Processing order matters and is:
#'
#' 1. Lowercase, trim, and turn `&`, `/`, `+` into spaces.
#' 2. German digraph expansion (a-umlaut to `ae`, eszett to `ss`, ...) when
#'    `"de"` is among `lang`. This runs *before* transliteration, which would
#'    otherwise reduce u-umlaut to `u` rather than `ue`.
#' 3. Transliteration of remaining accents to ASCII, so that accented legal
#'    forms (`Societe`, `Sarl`, `Societa` as actually written) are recognised.
#' 4. Apostrophes are deleted rather than split on, so Romance elisions and
#'    English possessives join up: `L'Oreal` and `Loreal` both give `loreal`,
#'    and `McDonald's` gives `mcdonalds`.
#' 5. Dotted acronyms are joined: `S.A.R.L.` becomes `sarl`, so it is removed
#'    as a legal form rather than left behind as four stray letters.
#' 6. Legal forms and noise words are removed, then punctuation and repeated
#'    whitespace are collapsed.
#'
#' Country names are deliberately **not** removed. They identify a firm as often
#' as they pad it: stripping them turns `Credit Suisse` into `credit` and
#' `Air France` into `air`, which can then exact-match an unrelated company.
#' Because a spurious exact match is trusted and never reaches the fuzzy stages
#' or the LLM validator, the safer default is to keep them. Structural branch
#' markers (`Niederlassung`, `Succursale`, `Filiale`) *are* removed. Pass
#' `extra_stopwords = c("deutschland", "schweiz")` to strip geography anyway.
#'
#' Normalization is deliberately lossy, so distinct firms can end up sharing a
#' normalized name. [match_companies()] handles that by grouping rather than
#' failing; see [dict_crosswalk()]. Names consisting only of stop words
#' normalize to `""`.
#'
#' Use [company_name_stopwords()] to inspect exactly which tokens are removed.
#'
#' @param x A character vector of company names.
#' @param lang Character vector of language codes whose conventions should be
#'   applied. Any of `"de"` (German), `"fr"` (French), `"it"` (Italian),
#'   `"en"` (English). Defaults to all four; narrow it if your data is
#'   single-language and you want to avoid over-stripping.
#' @param extra_stopwords Optional character vector of additional whole-word
#'   tokens to strip, applied after the built-in lists. Use it for
#'   corpus-specific filler, or to remove geography (`c("deutschland",
#'   "schweiz")`), which is kept by default.
#' @return A character vector of normalized names.
#' @seealso [company_name_stopwords()], [match_companies()]
#' @importFrom stringi stri_trans_general
#' @export
#' @examples
#' # German
#' normalize_company_name("BMW AG")
#' normalize_company_name("Siemens GmbH & Co. KG")
#' normalize_company_name("Gebr. Mueller Grosshandel AG")
#'
#' # French, including an accented legal form and an elision
#' normalize_company_name("Societe Generale S.A.")
#' normalize_company_name("L'Oreal France S.A.")
#'
#' # Italian
#' normalize_company_name("Ditta Rossi S.p.A.")
#'
#' # English
#' normalize_company_name("The Boring Company LLC")
#'
#' # Restrict to one language to strip less
#' normalize_company_name("Sa Casa di Roma", lang = "de")
#' normalize_company_name("Sa Casa di Roma", lang = c("fr", "it"))
#'
#' # Country names are kept by default, because they are often identifying
#' normalize_company_name("Credit Suisse AG")
#' # ... strip them explicitly if your data needs it
#' normalize_company_name("Toyota Deutschland GmbH", extra_stopwords = "deutschland")
normalize_company_name <- function(x,
                                   lang = c("de", "fr", "it", "en"),
                                   extra_stopwords = NULL) {
  lang <- match.arg(lang, several.ok = TRUE)

  if (length(x) == 0) {
    return(character(0))
  }

  x <- tolower(as.character(x))
  x <- trimws(x)

  # Standardize separators
  x <- gsub("[&/\\+]", " ", x)

  # German digraphs, before transliteration flattens them to bare vowels
  if ("de" %in% lang) {
    x <- gsub("\u00df", "ss", x) # Eszett
    x <- gsub("\u00e4", "ae", x) # a-umlaut
    x <- gsub("\u00f6", "oe", x) # o-umlaut
    x <- gsub("\u00fc", "ue", x) # u-umlaut
  }

  # Transliterate remaining accents so that accented legal forms
  # ("societe", "sarl") match the stop-word patterns below.
  x <- stringi::stri_trans_general(x, "Latin-ASCII")

  # Drop apostrophes so the parts join rather than split. This keeps Romance
  # elisions and English possessives close to how people type them without the
  # punctuation: "l'oreal" -> "loreal" (not "oreal", which would miss the very
  # common "Loreal"), "mcdonald's" -> "mcdonalds".
  x <- gsub("['\u2019\u02bc`]", "", x)

  # Join dotted acronyms: "s.a.r.l." -> "sarl", "g.m.b.h." -> "gmbh".
  # Only fires after a single letter, so "co. kg" and "ltd." are untouched.
  x <- gsub("\\b([a-z])\\.", "\\1", x)

  x <- gsub(.fm_stopword_pattern(.fm_legal_forms, lang), " ", x)
  x <- gsub(.fm_stopword_pattern(.fm_noise_words, lang), " ", x)

  if (length(extra_stopwords) > 0) {
    extra <- tolower(trimws(as.character(extra_stopwords)))
    extra <- unique(extra[nzchar(extra) & !is.na(extra)])
    if (length(extra) > 0) {
      extra <- extra[order(-nchar(extra), extra)]
      x <- gsub(paste0("\\b(", paste(extra, collapse = "|"), ")\\b"), " ", x)
    }
  }

  # Remove punctuation and non-alphanumeric (keep digits and spaces)
  x <- gsub("[^a-z0-9 ]", " ", x)

  # Collapse whitespace
  x <- gsub("\\s+", " ", x)

  trimws(x)
}
