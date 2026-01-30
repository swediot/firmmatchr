library(data.table)
library(stringi)

set.seed(42) # For reproducibility

# --- 1. Helper Functions for Distortion ---

# Function to simulate typos (swap, delete, insert char)
add_typo <- function(s) {
  n <- nchar(s)
  if (n < 4) return(s)
  
  type <- sample(1:3, 1)
  pos <- sample(2:(n-1), 1)
  
  if (type == 1) { # Swap
    paste0(substr(s, 1, pos-1), substr(s, pos+1, pos+1), substr(s, pos, pos), substr(s, pos+2, n))
  } else if (type == 2) { # Delete
    paste0(substr(s, 1, pos-1), substr(s, pos+1, n))
  } else { # Insert
    paste0(substr(s, 1, pos), sample(letters, 1), substr(s, pos+1, n))
  }
}

# Function to mess up legal forms
distort_legal <- function(s) {
  forms <- c(" GmbH", " AG", " KG", " SE", " Ltd")
  # 50% chance to drop legal form, 50% to change it or move it
  if (runif(1) > 0.5) {
    # Drop suffix
    gsub(paste(forms, collapse="|"), "", s)
  } else {
    # Add noise or change suffix
    paste0(gsub(paste(forms, collapse="|"), "", s), sample(c(" Deutschland", " Group", " & Co KG", " Service"), 1))
  }
}

# --- 2. Generate Clean Dictionary (1000 rows) ---
roots <- c("Tech", "Immo", "Bio", "Soft", "Data", "Bau", "Gastro", "Consult", "Fin", "Auto")
seconds <- c("Nova", "Sol", "Venture", "Sys", "Net", "Line", "Base", "Work", "Trust", "Mat")

# Create 1000 unique names
# We combine root + random string + legal form
clean_names <- unique(replicate(1200, {
  paste0(
    sample(roots, 1), 
    sample(seconds, 1), 
    " ", 
    stringi::stri_rand_strings(1, 4, pattern = "[A-Z]"), 
    sample(c(" GmbH", " AG", " SE", " KG"), 1)
  )
}))[1:1000]

dictionary <- data.table(
  orbis_id = 1:1000,
  company_name = clean_names,
  turnover = round(runif(1000, 1e5, 1e9))
)

# --- 3. Generate Dirty Queries (1000 rows) ---

# A. The Matches (500 rows)
# We take first 500 from dictionary and mess them up
matched_subset <- dictionary[1:500]
dirty_matches <- character(500)

for (i in 1:500) {
  clean <- matched_subset$company_name[i]
  
  # Apply random distortions
  r <- runif(1)
  
  if (r < 0.3) {
    # Case 1: Simple Lowercase + Typos
    dirty <- tolower(add_typo(clean))
  } else if (r < 0.6) {
    # Case 2: Legal Form Noise
    dirty <- distort_legal(clean)
  } else if (r < 0.8) {
    # Case 3: Heavy Noise (Prefix/Suffix)
    dirty <- paste("Firma", clean, "Germany")
  } else {
    # Case 4: Complex (Typo + Noise)
    dirty <- paste0(add_typo(distort_legal(clean)), " (Insolvenz)")
  }
  
  dirty_matches[i] <- dirty
}

# B. The Non-Matches (500 rows)
# Generate completely random names that look like companies but aren't in dict
dirty_non_matches <- replicate(500, {
  paste(
    stringi::stri_rand_strings(1, 8, pattern = "[a-z]"),
    "Services",
    sample(c("GmbH", "Limited"), 1)
  )
})

# Combine
queries <- data.table(
  id = 1:1000,
  company_name = c(dirty_matches, dirty_non_matches),
  true_orbis_id = c(matched_subset$orbis_id, rep(NA, 500)) # Ground Truth for checking
)

# Shuffle rows so they aren't in order
set.seed(123)
queries <- queries[sample(1:1000)]

# --- 4. Save to CSV ---
write.csv(dictionary, "dictionary.csv", row.names = FALSE)
write.csv(queries, "my_dirty_data.csv", row.names = FALSE)

cat("✅ Created 'dictionary.csv' (1000 rows) and 'my_dirty_data.csv' (1000 rows).\n")
cat("ℹ️  Approx. 500 rows in dirty data should match.\n")