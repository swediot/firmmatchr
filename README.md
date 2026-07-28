# firmmatchr: Robust Probabilistic Matching of Company Names

**firmmatchr** is an R package designed to match messy, user-generated company names (e.g., from surveys, resumes, or web scraping) against a clean, authoritative dictionary (e.g., Orbis, Commercial Registers). Normalization covers **German, French, Italian and English** conventions, which suits multilingual registers such as the Swiss one.

Unlike simple fuzzy matching, **firmmatchr** uses a cascading pipeline of four distinct algorithms to maximize recall while maintaining high precision. It also includes an optional LLM-based validation step (via Azure OpenAI) to verify doubtful matches.

---

## Key Features

- **Cascading Architecture**: Matches are found in stages. Easy matches are caught early; harder matches are passed to more complex engines.
- **Multilingual Normalization**: DE / FR / IT / EN legal forms (`GmbH`, `S.à r.l.`, `S.p.A.`, `LLC`), umlauts and accents, dotted acronyms, and noise words (Standort, Succursale, Filiale).
- **Lossless on Duplicates**: Normalization inevitably collapses distinct entries onto the same string. The pipeline groups them instead of failing, and keeps a crosswalk so every original entry stays traceable.
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
# Returns one row per matched query row, with columns:
#   query_id, query_name, dict_id, dict_name, match_type,
#   n_dict_ids, dict_id_all
```

### 2. Languages

Normalization applies German, French, Italian and English conventions by
default, so the same firm written four different ways converges:

```r
normalize_company_name("Müller Handels GmbH")     #> "mueller handels"
normalize_company_name("Société Générale S.A.")   #> "generale"
normalize_company_name("Ditta Rossi S.p.A.")      #> "rossi"
normalize_company_name("The Boring Company LLC")  #> "boring"

# Spelling variants converge on the same key
normalize_company_name(c("L'Oréal", "Loreal"))    #> "loreal" "loreal"
normalize_company_name(c("Nestlé S.A.", "NESTLE SA"))  #> "nestle" "nestle"
```

Three details worth knowing:

- **Accented legal forms are recognised.** Transliteration runs *before*
  stop-word removal, so `Société` and `Sàrl` are stripped correctly.
- **Dotted acronyms are joined**, not split: `S.A.R.L.` → `sarl` (then stripped),
  not four stray letters. A dot after a multi-letter token is still a separator,
  so `Co. KG` is unaffected.
- **Apostrophes join** rather than split: `L'Oréal` and `Loreal` both give
  `loreal`; `McDonald's` gives `mcdonalds`.

Identifying words are deliberately kept — `Deutsche Bank AG` normalizes to
`deutsche bank`, not `bank`. **Country names are kept too**, because they
identify a firm as often as they pad it:

```r
normalize_company_name("Credit Suisse AG")  #> "credit suisse"   (not "credit")
normalize_company_name("Air France")        #> "air france"      (not "air")
```

Stripping them would let those collapse onto unrelated firms — and a spurious
*exact* match is trusted, never reaching the fuzzy stages or the LLM validator.
Structural branch markers are still removed (`Bosch Niederlassung` → `bosch`).
If your data really is padded with geography, strip it explicitly:

```r
normalize_company_name("Toyota Deutschland GmbH", extra_stopwords = "deutschland")
#> "toyota"
```

Narrow the languages with `lang` if your data is single-language and the default
strips too much:

```r
normalize_company_name("Sa Casa di Roma", lang = "de")         #> "sa casa di roma"
normalize_company_name("Sa Casa di Roma", lang = c("fr", "it")) #> "casa roma"

# The same argument exists on the pipeline
match_companies(queries, dictionary, lang = c("de", "fr"))
```

To see exactly what gets removed:

```r
company_name_stopwords("it")
#>   lang       type  token
#> 1   it legal_form    spa
#> 2   it legal_form    srl
#> ...
nrow(company_name_stopwords())  #> 109
```

### 3. Duplicates After Normalization

Normalization is lossy on purpose — `"Meier GmbH"` and `"Meier AG"` both become
`"meier"`. Distinct dictionary rows therefore routinely collapse onto the same
string. `match_companies()` does **not** fail on this: it groups such rows,
matches against one representative, and keeps a full crosswalk.

```r
dictionary <- data.frame(
  orbis_id     = c("D1", "D2", "D3", "D4"),
  company_name = c("Meier GmbH", "Meier AG", "Meier Holding", "BMW AG")
)
queries <- data.frame(
  query_id     = c("q1", "q2"),
  company_name = c("Meier", "BMW")
)

results <- match_companies(queries, dictionary)
```

| query_id | query_name | dict_id | dict_name  | match_type | n_dict_ids | dict_id_all |
|----------|------------|---------|------------|------------|------------|-------------|
| q1       | Meier      | D1      | Meier GmbH | Perfect    | 3          | `D1\|D2\|D3` |
| q2       | BMW        | D4      | BMW AG     | Perfect    | 1          | `D4`        |

- `dict_id` is the **representative** (first) entry of the group.
- `dict_id_all` lists every entry in the group.
- `n_dict_ids > 1` flags a match you may want to disambiguate (e.g. with the LLM
  validator, or by revenue).

Two helpers recover the originals:

```r
# Mapping from every original dictionary row to its representative
dict_crosswalk(results)
#>   name_clean dict_id     dict_name dict_id_rep n_dict_ids is_representative
#> 1      meier      D1    Meier GmbH          D1          3              TRUE
#> 2      meier      D2      Meier AG          D1          3             FALSE
#> 3      meier      D3 Meier Holding          D1          3             FALSE
#> 4        bmw      D4        BMW AG          D4          1              TRUE

# One row per (query, original dictionary row)
expand_matches(results)
#>   query_id query_name dict_id     dict_name match_type dict_id_rep ...
#> 1       q1      Meier      D1    Meier GmbH    Perfect          D1
#> 2       q1      Meier      D2      Meier AG    Perfect          D1
#> 3       q1      Meier      D3 Meier Holding    Perfect          D1
#> 4       q2        BMW      D4        BMW AG    Perfect          D4
```

To restore the old strict behaviour and abort when duplicates appear, pass
`on_duplicate = "error"`.

Rows whose name is empty or `NA` **after** normalization (e.g. `"Holding Group"`,
which is nothing but stop words) are dropped with a warning — otherwise every
such row would exact-match every other one.

If your data is already cleaned, skip normalization per side with
`dict_norm = FALSE` and/or `query_norm = FALSE`.

### 4. LLM Validation (Optional)

If you have matches that are fuzzy but not certain, you can use the LLM module to have an AI act as a human verifier.

**Prerequisites**: Set your Azure OpenAI credentials in your `.Renviron` file:
```bash
AZURE_ENDPOINT="https://your-resource.openai.azure.com"
AZURE_API_KEY="your-secret-key"
AZURE_DEPLOYMENT="gpt-4-mini"
```

**Running Validation**:
```r
# The results already carry both names, so no joining back is needed.
validated <- validate_matches_llm(
  data = results,
  query_name_col = "query_name",   # The messy name
  dict_name_col = "dict_name",     # The official dictionary name
  output_dir = "llm_cache",        # Saves progress in case of crash
  batch_size = 50
)

# Output includes 'LLM_decision' (CORRECT/INCORRECT) and 'LLM_reason'

# To have the LLM adjudicate between the members of a collapsed group,
# validate the expanded table instead:
validated_all <- validate_matches_llm(
  data = expand_matches(results),
  query_name_col = "query_name",
  dict_name_col = "dict_name"
)
```

---

## How It Works (The Cascade)

The `match_companies` function processes unmatched records through four engines in order:

1. **Exact Match**:
   - Joins on the normalized string (lowercase, DE/FR/IT/EN cleaning).
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

All four engines run on **distinct normalized names**, not on raw rows. Each
group of duplicates is matched once, and the results are fanned back out to the
original rows at the end.

---

## Example

See `0_run_package.R` for an example usage with synthetic data. 

---

## License

MIT License. See LICENSE for details.

---

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.
