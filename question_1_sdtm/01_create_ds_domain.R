# ==============================================================================
# SDTM DS (Disposition) Domain Creation Script
# ==============================================================================
#
# Purpose: Create SDTM DS domain from raw disposition data using {sdtm.oak}
# Author: Ousmane Diallo
# Date: 2026-02-03
#
# Description:
# This script transforms raw disposition data into SDTM DS domain format
# following CDISC standards. It handles controlled terminology mapping,
# date/time parsing, and derives required SDTM variables.
#
# Business Rules:
# 1. OTHERSP Priority: When OTHERSP (verbatim text) is non-empty:
#    - DSTERM = OTHERSP (verbatim disposition term)
#    - DSDECOD = OTHERSP (decoded disposition term)
# 2. Default Mapping: When OTHERSP is empty:
#    - DSTERM = IT.DSTERM (investigator term)
#    - DSDECOD = CT mapping of IT.DSDECOD (standardized code)
# 3. DSCAT Derivation:
#    - If IT.DSDECOD = "Randomized" → DSCAT = "PROTOCOL MILESTONE"
#    - Else if OTHERSP non-empty → DSCAT = "OTHER EVENT"
#    - Else → DSCAT = "DISPOSITION EVENT"
# 4. Date/Time Format: Parse MM-DD-YYYY + HH:MM → ISO8601 (YYYY-MM-DDTHH:MM)
# 5. Study Day: Calculate relative to reference start date (RFSTDTC)
#
# Input Files:
# - pharmaverseraw::ds_raw - Raw disposition data
# - pharmaversesdtm::dm - Demographics domain
# - sdtm_ct.csv - Controlled terminology specifications
#
# Output Files:
# - ds.rds - SDTM DS domain (R binary format)
# - ds.csv - SDTM DS domain (CSV format)
# - qc_summary.csv - Quality control metrics
# - ct_mapping_messages.txt - Controlled terminology mapping log
# - session_info.txt - R session and package version information
#
# Required Packages:
# - dplyr: Data manipulation
# - stringr: String operations
# - sdtm.oak: SDTM transformation utilities
# - pharmaverseraw: Raw study data
# - pharmaversesdtm: SDTM reference domains
#
# ==============================================================================

# ── SECTION 0: CONFIGURATION AND SETUP ────────────────────────────────────────

# Define output directories
OUTPUT_DIR <- "question_1_sdtm/output"
INPUT_DIR  <- "question_1_sdtm/input"

# Create directories if they don't exist
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(INPUT_DIR,   recursive = TRUE, showWarnings = FALSE)

# Log file (evidence code runs error-free)
log_file <- file.path(OUTPUT_DIR, "run_log.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

# Package management
pkgs <- c("dplyr", "stringr", "sdtm.oak", "pharmaverseraw", "pharmaversesdtm")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
}

# Load packages with suppressed startup messages
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(sdtm.oak)
  library(pharmaverseraw)
  library(pharmaversesdtm)
})


cat("=", rep("=", 78), "\n", sep = "")
cat("SDTM DS Domain Creation\n")
cat("=", rep("=", 78), "\n", sep = "")
cat("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=", rep("=", 78), "\n\n", sep = "")

# ── HELPER FUNCTIONS ───────────────────────────────────────────────────────────

#' Normalize Values for CT Matching
#'
#' Standardizes text values to improve controlled terminology matching.
#' Applies trimming, case normalization, and specific term mappings.
#'
#' @param x Character vector to normalize
#' @return Normalized character vector
#'
#' @examples
#' normalize_val(" Screen Failure ") # Returns "trial screen failure"

normalize_val <- function(x) {
  x <- trimws(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_to_lower(x)
  
  # exact replacements
  x <- dplyr::if_else(x == "screen failure", "trial screen failure", x)
  x <- dplyr::if_else(x == "completed", "complete", x)
  x
}

# ── SECTION 1: DATA LOADING WITH VALIDATION ───────────────────────────────────

cat("\n=== SECTION 1: Loading Data ===\n\n")

# Load raw disposition data
ds_raw <- pharmaverseraw::ds_raw
cat("Loaded ds_raw: ", nrow(ds_raw), " records\n")

# Load demographics domain
dm <- pharmaversesdtm::dm
cat("Loaded dm: ", nrow(dm), " subjects\n")

# Load controlled terminology (CT) with error handling
study_ct <- tryCatch(
  {
    utils::read.csv(
      file.path(INPUT_DIR, "sdtm_ct.csv"),
      stringsAsFactors = FALSE
    ) |>
      dplyr::mutate(across(where(is.character), ~ str_trim(.x)))
  },
  error = function(e) {
    stop(
      "Failed to load CT file: ", e$message,
      "\nExpected location: ", file.path(INPUT_DIR, "sdtm_ct.csv")
    )
  }
)
cat("Loaded study_ct: ", nrow(study_ct), " terms\n")

# Filter CT for DS domain
ct_ds <- study_ct |>
  dplyr::filter(codelist_code == "C66727")

cat("DS domain CT terms: ", nrow(ct_ds), "\n")

# Get OAK ID variables
idv <- oak_id_vars()

# ── SECTION 2: DATA CLEANING AND OTHERSP PRIORITY ─────────────────────────────

cat("\n=== SECTION 2: Data Cleaning ===\n\n")

# Clean and prepare raw data
ds_raw <- ds_raw |>
  mutate(
    
    # Trim whitespace and convert to character
    across(
      c(IT.DSTERM, OTHERSP, IT.DSDECOD, DSDTCOL, DSTMCOL),
      ~ str_trim(as.character(.x))
    ),
    
    # Convert empty strings to NA
    OTHERSP = na_if(OTHERSP, ""),
    
    # BUSINESS RULE: OTHERSP Priority
    # When OTHERSP is present, it takes priority over investigator terms
    DSTERM_SRC = if_else(
      !is.na(OTHERSP),
      OTHERSP,
      IT.DSTERM
    ),
    DSDECOD_SRC = if_else(
      !is.na(OTHERSP),
      OTHERSP,
      IT.DSDECOD
    )
  ) |>
  # Filter: Keep only records with at least one disposition term
  filter(
    !(is.na(DSTERM_SRC) & is.na(DSDECOD_SRC))
  )

cat("Records after cleaning: ", nrow(ds_raw), "\n")

# Validate we still have data
if (nrow(ds_raw) == 0) {
  stop("No valid records remaining after cleaning")
}

# ── SECTION 3: GENERATE OAK IDENTIFIERS ───────────────────────────────────────

cat("\n=== SECTION 3: Generating OAK IDs ===\n\n")

# Generate unique identifiers for record tracking
ds_raw <- ds_raw |>
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

cat("OAK IDs generated\n")

# ── SECTION 4: MAP DSTERM ──────────────────────────────────────────────────────

cat("\n=== SECTION 4: Mapping DSTERM ===\n\n")

# Direct assignment (no CT needed since OTHERSP priority already handled)
ds_dsterm <- assign_no_ct(
  raw_dat = ds_raw,
  raw_var = "DSTERM_SRC",
  tgt_var = "DSTERM",
  id_vars = idv
) |>
  select(all_of(idv), DSTERM, patient_number)

cat("DSTERM mapped for ", nrow(ds_dsterm), " records\n")

# ── SECTION 5: MAP DSDECOD WITH CT ────────────────────────────────────────────

cat("\n=== SECTION 5: Mapping DSDECOD with CT ===\n\n")

# Normalize DSDECOD values for better CT matching

ds_raw <- ds_raw |>
  dplyr::mutate(
    DSDECOD_SRC_NORM = normalize_val(DSDECOD_SRC),
    DSDECOD_SRC_NORM = dplyr::case_when(
      DSDECOD_SRC_NORM %in% c("death", "dead") ~ "dead",
      TRUE ~ DSDECOD_SRC_NORM
    )
  )

# Normalize CT values
ct_ds_norm <- ct_ds |>
  mutate(collected_value = normalize_val(collected_value))

# Build allowed CT values for the DSDECOD codelist (C66727)
ct_allowed <- ct_ds_norm |>
  filter(codelist_code == "C66727") |>
  pull(collected_value) |>
  unique()

# Exclude values that are NOT DS disposition outcomes (visits) 
# Keep "randomized" as unmapped (Option A), so we do NOT exclude it here
exclude_not_ds_outcomes <- c("final lab visit", "final retrieval visit")

# Prepare the data sent to assign_ct()
ds_for_ct <- ds_raw |>
  filter(
    DSDECOD_SRC_NORM %in% ct_allowed,                 
    !DSDECOD_SRC_NORM %in% exclude_not_ds_outcomes
  )

ds_dsdecod <- withCallingHandlers(
  assign_ct(
    raw_dat = ds_for_ct,
    raw_var = "DSDECOD_SRC_NORM",
    tgt_var = "DSDECOD",
    ct_spec = ct_ds_norm,
    ct_clst = "C66727",
    id_vars = idv
  )) |>
  select(dplyr::all_of(idv), DSDECOD)

# Merge ds_dsdecod back to the full ds_raw (preserve all rows) 
# Rows not sent to assign_ct() (e.g., randomized, final lab visit)
ds_raw <- ds_raw |>
  left_join(ds_dsdecod, by = idv)


# ── SECTION 6: DERIVE DSCAT ────────────────────────────────────────────────────

cat("\n=== SECTION 6: Deriving DSCAT ===\n\n")

# BUSINESS RULE: DSCAT Derivation Logic
# Priority 1: Randomized → PROTOCOL MILESTONE
# Priority 2: OTHERSP present → OTHER EVENT
# Priority 3: Default → DISPOSITION EVENT
ds_dscat <- ds_raw |>
  mutate(
    DSCAT = case_when(
      # Priority 1: Protocol milestone for randomization
      !is.na(IT.DSDECOD) & IT.DSDECOD == "Randomized" ~ "PROTOCOL MILESTONE",
      
      # Priority 2: Other event for verbatim text
      !is.na(OTHERSP) & nchar(OTHERSP) > 0 ~ "OTHER EVENT",
      
      # Priority 3: Default disposition event
      TRUE ~ "DISPOSITION EVENT"
    )
  ) |>
 select(all_of(idv), DSCAT)

# Log DSCAT distribution
dscat_summary <- ds_dscat |>
  count(DSCAT, name = "n") |>
  arrange(desc(n))

cat("DSCAT distribution:\n")
print(dscat_summary)

# ── SECTION 7: PARSE DATE/TIME ─────────────────────────────────────────────────

cat("\n=== SECTION 7: Parsing Date/Time ===\n\n")

# Parse MM-DD-YYYY and HH:MM to ISO8601 format
ds_dtc <- ds_raw |>
  mutate(
    # Parse date component
    date_parsed = case_when(
      grepl("^\\d{2}-\\d{2}-\\d{4}$", DSDTCOL) ~ as.Date(DSDTCOL, format = "%m-%d-%Y"),
      TRUE ~ as.Date(NA)
    ),
    
    # Parse and validate time component
    time_parsed = case_when(
      grepl("^\\d{1,2}:\\d{2}$", DSTMCOL) &
        between(as.integer(sub(":.*", "", DSTMCOL)), 0, 23) &
        between(as.integer(sub(".*:", "", DSTMCOL)), 0, 59) ~
        sprintf(
          "%02d:%02d",
          as.integer(sub(":.*", "", DSTMCOL)),
          as.integer(sub(".*:", "", DSTMCOL))
        ),
      TRUE ~ NA_character_
    ),
    
    # Combine into ISO8601 format
    DSDTC = case_when(
      !is.na(date_parsed) & !is.na(time_parsed) ~ paste0(format(date_parsed, "%Y-%m-%d"), "T", time_parsed),
      !is.na(date_parsed) ~ format(date_parsed, "%Y-%m-%d"),
      !is.na(time_parsed) ~ paste0("T", time_parsed),
      TRUE ~ NA_character_
    )
  ) |>
  select(all_of(idv), DSDTC)

# Log parsing statistics
n_full_dt <- sum(grepl("T", ds_dtc$DSDTC, fixed = TRUE), na.rm = TRUE)
n_date_only <- sum(!grepl("T", ds_dtc$DSDTC, fixed = TRUE) &
                     !is.na(ds_dtc$DSDTC), na.rm = TRUE)
n_missing <- sum(is.na(ds_dtc$DSDTC))

cat("Date/time parsing:\n")
cat("  Full datetime: ", n_full_dt, "\n")
cat("  Date only: ", n_date_only, "\n")
cat("  Missing: ", n_missing, "\n")

# ── SECTION 8: MERGE DATASETS ──────────────────────────────────────────────────

cat("\n=== SECTION 8: Merging Datasets ===\n\n")

# Prepare DM for joining
dm_keyed <- dm |>
  transmute(
    patient_number = sub("^[^-]+-", "", USUBJID),
    STUDYID,
    USUBJID,
    RFSTDTC
  )

# Merge all components
ds <- ds_dsterm |>
  left_join(ds_dsdecod, by = idv) |>
  left_join(ds_dscat,   by = idv) |>
  left_join(ds_dtc,     by = idv) |>
  left_join(dm_keyed,   by = "patient_number") |>
  mutate(
    DOMAIN = "DS",
    VISIT = NA_character_,
    VISITNUM = NA_real_,
    DSSTDTC = DSDTC
  )

# Validate critical variables
stopifnot(
  "Critical variables missing" =
    all(c("STUDYID", "USUBJID", "DSDTC") %in% names(ds))
)

cat("Datasets merged successfully\n")

# ── SECTION 9: DERIVE DSSEQ AND STUDY DAY ─────────────────────────────────────

cat("\n=== SECTION 9: Deriving DSSEQ and Study Day ===\n\n")

ds_final <- ds |>
  # Sort for consistent sequence assignment
  arrange(USUBJID, DSDTC, DSTERM, DSDECOD) |>
  
  # Derive sequence number
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = "USUBJID"
  ) |>
  
  # Calculate study day
  mutate(
    DSSTDTC_DATE = as.Date(substr(DSSTDTC, 1, 10), "%Y-%m-%d"),
    RFSTDTC_DATE = as.Date(substr(RFSTDTC, 1, 10), "%Y-%m-%d"),
    
    # CDISC rule: No day 0, +1 if on/after reference date
    DSSTDY = case_when(
      is.na(DSSTDTC_DATE) | is.na(RFSTDTC_DATE) ~ NA_integer_,
      DSSTDTC_DATE >= RFSTDTC_DATE ~
        as.integer(DSSTDTC_DATE - RFSTDTC_DATE) + 1L,
      TRUE ~ as.integer(DSSTDTC_DATE - RFSTDTC_DATE)
    )
  ) |>
  
  # Select final variables in order
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ,
    DSTERM, DSDECOD, DSCAT,
    VISITNUM, VISIT,
    DSDTC, DSSTDTC, DSSTDY
  ) |>
  
  # Final sort
  arrange(USUBJID, DSSEQ)

cat("DSSEQ and DSSTDY derived\n")

# ── SECTION 10: QUALITY CONTROL ───────────────────────────────────────────────

cat("\n=== SECTION 10: Quality Control ===\n\n")

# Generate QC metrics
qc <- ds_final |>
  summarise(
    n_records = n(),
    n_subjects = n_distinct(USUBJID),
    n_na_usubjid = sum(is.na(USUBJID)),
    n_na_dsterm = sum(is.na(DSTERM)),
    n_na_dsdecod = sum(is.na(DSDECOD)),
    n_na_dscat = sum(is.na(DSCAT)),
    n_na_dsdtc = sum(is.na(DSDTC)),
    n_na_dsstdy = sum(is.na(DSSTDY)),
    min_dsstdy = suppressWarnings(min(DSSTDY, na.rm = TRUE)),
    max_dsstdy = suppressWarnings(max(DSSTDY, na.rm = TRUE)),
    pct_complete_dsterm = round(100 * (1 - n_na_dsterm / n_records), 1),
    pct_complete_dsdecod = round(100 * (1 - n_na_dsdecod / n_records), 1),
    pct_complete_dscat = round(100 * (1 - n_na_dscat / n_records), 1)
  )

cat("QC Summary:\n")
print(qc)

# Check for duplicate keys
dup_check <- ds_final |>
  count(USUBJID, DSSEQ) |>
  filter(n > 1)

if (nrow(dup_check) > 0) {
  warning("Found ", nrow(dup_check), " duplicate USUBJID-DSSEQ combinations")
} else {
  cat("\n✓ No duplicate keys found\n")
}

# Save QC summary
write.csv(
  qc,
  file.path(OUTPUT_DIR, "qc_summary.csv"),
  row.names = FALSE
)

# ── SECTION 11: SAVE OUTPUTS ───────────────────────────────────────────────────

cat("\n=== SECTION 11: Saving Outputs ===\n\n")

# Save DS domain
saveRDS(
  ds_final,
  file.path(OUTPUT_DIR, "ds.rds")
)
cat("✓ Saved: ", file.path(OUTPUT_DIR, "ds.rds"), "\n")

write.csv(
  ds_final,
  file.path(OUTPUT_DIR, "ds.csv"),
  row.names = FALSE
)
cat("✓ Saved: ", file.path(OUTPUT_DIR, "ds.csv"), "\n")

# Save session info
capture.output(
  sessionInfo(),
  file = file.path(OUTPUT_DIR, "session_info.txt")
)
cat("✓ Saved: ", file.path(OUTPUT_DIR, "session_info.txt"), "\n")

# ── SECTION 12: FINAL SUMMARY ──────────────────────────────────────────────────

cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("DS DOMAIN CREATION COMPLETED\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

cat("Final Statistics:\n")
cat("  Total records: ", nrow(ds_final), "\n")
cat("  Unique subjects: ", n_distinct(ds_final$USUBJID), "\n")
cat("  DSTERM complete: ", qc$pct_complete_dsterm, "%\n")
cat("  DSDECOD complete:", qc$pct_complete_dsdecod, "%\n")
cat("  DSCAT complete: ", qc$pct_complete_dscat, "%\n")

cat("\nOutput Files:\n")
cat("  ", file.path(OUTPUT_DIR, "ds.rds"), "\n")
cat("  ", file.path(OUTPUT_DIR, "ds.csv"), "\n")
cat("  ", file.path(OUTPUT_DIR, "qc_summary.csv"), "\n")
cat("  ", file.path(OUTPUT_DIR, "ct_mapping_messages.txt"), "\n")
cat("  ", file.path(OUTPUT_DIR, "session_info.txt"), "\n")

cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("Completed at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

# End of script