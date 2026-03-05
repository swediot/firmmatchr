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
