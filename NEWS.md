# firmmatchr 0.2.0

## Multilingual normalization (breaking change)

`normalize_company_name()` previously applied German conventions only. It now
covers **German, French, Italian and English**, which is what multilingual
registers such as the Swiss one require.

* New `lang` argument on `normalize_company_name()` and `match_companies()`,
  accepting any of `"de"`, `"fr"`, `"it"`, `"en"`. Defaults to all four; narrow
  it for single-language data to strip fewer tokens. **The default therefore
  strips more than in 0.1.x** — pass `lang = "de"` for the old scope.
* Legal forms and noise words are now organised per language and cover ~109
  tokens (`sarl`, `sasu`, `eurl`, `societe`, `srl`, `snc`, `scarl`, `societa`,
  `llp`, `incorporated`, `succursale`, `niederlassung`, ...).
* New `company_name_stopwords()` returns exactly which tokens are stripped for
  a given set of languages, so the normalizer can be audited.
* Fixed the processing order: transliteration now runs **before** stop-word
  removal. Accented legal forms such as `Société` and `Sàrl` were previously
  never recognised on the first pass, because the patterns were matched against
  the still-accented string.
* **`normalize_company_name()` is now idempotent** — applying it to its own
  output is a no-op, so one pass always suffices. This follows from the
  ordering fix above: previously `Nestlé Sàrl` became `nestle sarl` on the
  first pass and only lost `sarl` on the second, so callers had to re-normalize
  in a loop until the result stopped changing. **Such loops can now be replaced
  by a single call.** A regression test locks the property in.
* Dotted acronyms are now joined rather than split: `S.A.R.L.` becomes `sarl`
  (and is stripped) instead of four stray letters `s a r l`. Dots after
  multi-letter tokens still act as separators, so `Co. KG` is unaffected.
* Apostrophes are now deleted rather than turned into spaces, so elisions and
  possessives join up: `L'Oréal` and `Loreal` both normalize to `loreal`, and
  `McDonald's` gives `mcdonalds` rather than `mcdonald s`.
* Identifying words are deliberately preserved: `Deutsche Bank` still normalizes
  to `deutsche bank`, not `bank`.
* **Country names are no longer stripped** (0.1.x removed `deutschland` and
  `germany`). They identify a firm as often as they pad it: removing them turned
  `Credit Suisse` into `credit` and `Air France` into `air`, either of which can
  then exact-match an unrelated company. Since a spurious exact match is trusted
  and never reaches the fuzzy stages or the LLM validator, keeping them is the
  safer default. Structural branch markers (`Niederlassung`, `Succursale`,
  `Filiale`, `Standort`) are still removed.
* New `extra_stopwords` argument on `normalize_company_name()` and
  `match_companies()` for corpus-specific filler — use it to strip geography
  anyway, e.g. `extra_stopwords = c("deutschland", "schweiz")`.

## Duplicate handling (breaking change)

`normalize_company_name()` is lossy by design: `"Meier GmbH"` and `"Meier AG"`
both become `"meier"`. Previously `match_companies()` aborted whenever
normalization produced duplicate dictionary names, which made it unusable on
real dictionaries without hand-written pre-cleaning.

* `match_companies()` now **collapses** duplicate normalized names instead of
  aborting. Each group is matched once against a representative row, and no
  original row is lost:
    * `dict_id` in the result is the representative (first) id of the group.
    * New `dict_id_all` column lists every id in the group, separated by `"|"`.
    * New `n_dict_ids` column reports the group size (`1` when unambiguous).
* New `dict_crosswalk()` returns the mapping from every original dictionary row
  to its representative, so collapsed entries always remain traceable.
* New `expand_matches()` expands a result to one row per original dictionary
  row, recovering the source strings behind a collapsed group.
* New `on_duplicate` argument. The default `"collapse"` is the behaviour above;
  `"error"` restores the previous strict abort.
* Duplicate query names — and duplicate query ids — are now handled too. Each
  distinct normalized query name is matched once and the result is fanned back
  out, so every input row still gets its own output row.
* Rows whose name is empty or `NA` after normalization are now dropped with a
  warning. Previously an empty string could "exactly match" every other empty
  string, producing spurious matches.

## Other changes

* New `dict_norm` and `query_norm` arguments let you skip normalization on
  either side when the input is already clean.
* `match_companies()` now returns `query_name` and `dict_name` alongside the
  ids, so the result is readable (and ready for `validate_matches_llm()`)
  without joining the inputs back on. **The set of returned columns has changed;
  code that relied on the previous three-column output may need updating.**
* Fixed an error when the exact-match engine found no matches (the `match_type`
  column was referenced before it was created). All engines now return a
  consistently typed empty result.
* The matching engines no longer add helper columns (`block`, `fts_q`) to their
  inputs by reference.
* Removed `LazyData` from `DESCRIPTION` (the package ships no `data/`
  directory); added `URL` and `BugReports`. Fixed a dead reference URL and
  retitled the package, which is no longer German-only.

# firmmatchr 0.1.3

* New Features:
    * Added standard OpenAI-compatible support to `validate_matches_llm` enabling it to connect to local LLM instances (like Ollama or LM Studio).
    * Added an `engine` argument allowing choice between `"azure"`, `"openai"`, or `"local"`.
    * Extended documentation to cover local LLM usage with code examples.

# firmmatchr 0.1.2

* Fix CRAN Submission Issues:
    * Use single quotes for software/package names in DESCRIPTION.
    * Add references for 'zoomerjoin' and 'SQLite'/'FTS5' in DESCRIPTION.
    * Ensure `validate_matches_llm` writes to `tempdir()` by default.
    * Ensure exported function examples are executable.

# firmmatchr 0.1.1

* Fix CRAN check NOTEs:
    * Quote technical terms in DESCRIPTION to avoid spell check warnings.
    * Add cran-comments.md to .Rbuildignore.

# firmmatchr 0.1.0

* Initial release.
* Features:
    * Exact matching
    * Fuzzy matching (zoomerjoin)
    * FTS5 search (SQLite)
    * Rarity weighted matching
