## Test environments
* local Windows 11, R 4.6.1
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Submission notes
This is a new feature/bug-fix release (0.2.0), not a resubmission of a rejected
version.

Two substantive changes, both reported by users:

* `normalize_company_name()` now supports German, French, Italian and English
  conventions (new `lang` argument) rather than German only, and the package
  title was updated accordingly. New `company_name_stopwords()` exposes which
  tokens are stripped.
* `normalize_company_name()` is lossy, so distinct dictionary entries can
  normalize to the same string, and `match_companies()` previously aborted in
  that case. It now groups such entries, matches each group once, and returns a
  crosswalk (`dict_crosswalk()`, `expand_matches()`) so every original entry
  stays traceable.

See NEWS.md. Because the default normalization scope and the set of returned
columns both changed, the minor version was bumped.

## Notes
* Spelling: 'Orbis', 'zoomerjoin', 'Ollama' and 'FTS5' are technical terms or
  product names. They are quoted in the Description and listed in
  inst/WORDLIST.
* Examples that call the matching pipeline are wrapped in `\donttest{}` because
  they use the multi-threaded Rust internals of 'zoomerjoin'.
* `validate_matches_llm()` requires network access and API credentials, so its
  example is wrapped in `\dontrun{}`. It writes to `tempdir()` by default.
