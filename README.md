# firmmatchr: Robust Probabilistic Matching for German Company Names

**firmmatchr** is an R package designed to match messy, user-generated company names (e.g., from surveys, resumes, or web scraping) against a clean, authoritative dictionary (e.g., Orbis, Commercial Registers).

Unlike simple fuzzy matching, **firmmatchr** uses a cascading pipeline of four distinct algorithms to maximize recall while maintaining high precision. It also includes an optional LLM-based validation step (via Azure OpenAI) to verify doubtful matches.

---

## Key Features

- **Cascading Architecture**: Matches are found in stages. Easy matches are caught early; harder matches are passed to more complex engines.
- **German-Specific Normalization**: Handles Umlauts, legal forms (GmbH, AG, GmbH & Co. KG), and noise words (Standort, Germany).
- **High Performance**: Built on `data.table` and `RSQLite` (FTS5) for speed.
- **LLM Validator**: An integrated module to double-check "fuzzy" matches using GPT-4 models.

---

## Installation

You can install the package directly from GitHub (after you push it) or locally:
```r
# Install devtools if you haven't
install.packages("devtools")

# Install from GitHub
devtools::install_github("swediot/firmmatchr")

# Or install locally
devtools::install("path/to/firmmatchr")
```

---

## Usage

### 1. Basic Matching Pipeline

The core function `match_companies` runs the full cascade.
```r
library(firmmatchr)
library(data.table)

# 1. Load Data
# 'queries': Your messy data (e.g. from a survey)
# 'dictionary': Your clean reference list (e.g. Orbis)
queries <- fread("data/survey_results.csv")
dictionary <- fread("data/orbis_germany.csv")

# 2. Run the Pipeline
results <- match_companies(
  queries = queries,
  dictionary = dictionary,
  
  # Column mapping
  query_col = "employer_name",    # Column in 'queries'
  unique_id_col = "response_id",  # ID in 'queries'
  dict_col = "company_name",      # Column in 'dictionary'
  dict_id_col = "orbis_id",       # ID in 'dictionary'
  
  # Thresholds (Tunable)
  threshold_jw = 0.85,      # Strictness for Fuzzy Matching (0-1)
  threshold_zoomer = 0.4,   # Looseness for Blocking (Lower = more candidates)
  threshold_rarity = 1.0    # Threshold for TF-IDF style matching
)

# 3. View Results
print(results)
# Returns: query_id, dict_id, match_type (Perfect, Fuzzy, Rarity, etc.)
```

### 2. LLM Validation (Optional)

If you have matches that are fuzzy but not certain, you can use the LLM module to have an AI act as a human verifier.

**Prerequisites**: Set your Azure OpenAI credentials in your `.Renviron` file:
```bash
AZURE_ENDPOINT="https://your-resource.openai.azure.com"
AZURE_API_KEY="your-secret-key"
AZURE_DEPLOYMENT="gpt-4-mini"
```

**Running Validation**:
```r
# 1. Join names back to the results (The LLM needs text, not IDs!)
results_full <- merge(results, queries, by.x="query_id", by.y="response_id")
results_full <- merge(results_full, dictionary, by.x="dict_id", by.y="orbis_id")

# 2. Run Validation
validated <- validate_matches_llm(
  data = results_full,
  query_name_col = "employer_name",    # The messy name
  dict_name_col = "company_name",      # The official dictionary name
  output_dir = "llm_cache",            # Saves progress in case of crash
  batch_size = 50
)

# Output includes 'LLM_decision' (CORRECT/INCORRECT) and 'LLM_reason'
```

---

## How It Works (The Cascade)

The `match_companies` function processes unmatched records through four engines in order:

1. **Exact Match**:
   - Joins on the normalized string (lowercase, specialized cleaning).
   - Result: Highest confidence.

2. **Fuzzy Zoomer (Blocking)**:
   - Uses `zoomerjoin` (LSH/MinHash) to find candidates even with spelling errors.
   - Refined by Jaro-Winkler distance.
   - Best for: Typos like "Daimler" vs "Diamler".

3. **FTS5 (Search Engine)**:
   - Creates an in-memory SQLite database with Full-Text Search 5.
   - Allows for token-based matching (e.g., finding "Bosch" inside "Robert Bosch GmbH").
   - Best for: Missing words or reordered tokens.

4. **Rarity Weighted (TF-IDF)**:
   - Calculates the "rareness" of every word in the dictionary.
   - Matches are scored based on the sum of normalized weights of shared rare tokens.
   - Best for: Companies with unique names that are heavily misspelled or where word order varies significantly.

---

## Example

See `0_run_package.R` for an example usage with synthetic data. 

---

## License

MIT License. See LICENSE for details.

---

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.
