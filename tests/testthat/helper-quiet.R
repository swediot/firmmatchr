# The test fixtures are deliberately tiny, which makes 'zoomerjoin' emit an
# LSH-tuning warning ("a pair of records at the threshold have only an x%
# chance of being compared"). That advice is useful to real users, so it is not
# suppressed inside the package -- only here, to keep check output clean.
quiet_match <- function(...) {
    suppressWarnings(suppressMessages(match_companies(...)))
}
