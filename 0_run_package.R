devtools::document()  # Generates the documentation and NAMESPACE
devtools::install()   # Installs the package to your computer

library(firmmatchr)
library(data.table)
library(dplyr)

Sys.setenv(
  AZURE_ENDPOINT   = "your-endpoint",
  AZURE_API_KEY    = "your-key",
  AZURE_DEPLOYMENT = "gpt-4.1-mini",
  AZURE_API_VERSION = "2024-04-14"
)

# Load your data
queries <- fread("test data/my_dirty_data.csv")
dictionary   <- fread("test data/dictionary.csv")

# Pre-process dictionary (Essential: The package will error if the dictionary has duplicates!)

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

# 2. Join the official names back
# The LLM needs to see "MyQueryName" vs "Orbis Official Name"
matches[, query_id := as.numeric(query_id)]
matches[, dict_id := as.numeric(dict_id)]

matches_full <- matches %>%
  left_join(dictionary, by = c("dict_id" = "orbis_id")) %>%
  rename(company_name_dict = company_name)  %>%
  left_join(queries, by = c("query_id" = "id")) %>%
  rename(company_name_orig = company_name)

# 3. Run Validation
# It automatically picks up keys from .Renviron
checked_data <- validate_matches_llm(
  data = matches_full,
  query_name_col = "company_name_orig",
  dict_name_col = "company_name_dict",
  output_dir = "llm",
  batch_size = 50
)

# 4. Save
write.csv(checked_data, "test data/final_validated_matches.csv")
