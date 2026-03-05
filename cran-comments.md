## Test environments
* local macOS, R 4.x
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes

* This is a resubmission. 

## Response to CRAN Feedback
* **Local LLM Support**: Added support for standard OpenAI-compatible endpoints to `validate_matches_llm()`, fulfilling user requests to allow local inference (e.g., Ollama) alongside Azure OpenAI.
* **DESCRIPTION Formatting**: I have enclosed all software names in single quotes as requested.
* **Examples**: Executable examples are provided in `.Rd` files (default to tempdir output).

## Notes
* Spelling: 'Orbis', 'zoomerjoin', 'Ollama', and 'FTS5' are technical terms/product names. They have been quoted in the Description and documentation.
