devtools::document()  # Generates the documentation and NAMESPACE
devtools::install()   # Installs the package to your computer

library(firmmatchr)
library(data.table)

Sys.setenv(
  AZURE_ENDPOINT   = "your-endpoint",
  AZURE_API_KEY    = "your-key",
  AZURE_DEPLOYMENT = "gpt-4.1-mini",
  AZURE_API_VERSION = "2024-04-14"
)

# Load your data
queries    <- fread("test data/my_dirty_data.csv")
dictionary <- fread("test data/dictionary.csv")

# No pre-cleaning of the dictionary is required. Entries that collapse onto the
# same normalized name are grouped automatically and remain recoverable via
# dict_crosswalk() / expand_matches().

# 1. Run matching script
matches <- match_companies(
  queries = queries,
  dictionary = dictionary,
  query_col = "company_name",
  unique_id_col = "id",
  dict_col = "company_name",
  dict_id_col = "orbis_id",
  threshold_jw = 0.85,
  threshold_zoomer = 0.6
)

# 2. Inspect ambiguous matches (several dictionary entries share one name)
matches[n_dict_ids > 1]
expand_matches(matches)          # one row per original dictionary entry
dict_crosswalk(matches)          # full dictionary mapping

# 3. Run Validation
# The result already carries both names, so no joining back is needed.
# It automatically picks up keys from .Renviron
checked_data <- validate_matches_llm(
  data = matches,
  query_name_col = "query_name",
  dict_name_col = "dict_name",
  output_dir = "llm",
  batch_size = 50
)

# 4. Save
write.csv(checked_data, "test data/final_validated_matches.csv", row.names = FALSE)
