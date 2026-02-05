# ==============================================================================
# ADaM ADSL (Subject-Level Analysis Dataset) Creation
# ==============================================================================
#
# Program:     create_adsl.R
# Purpose:     Create ADSL dataset from SDTM sources
# Author:      Ousmane Diallo
# Date:        February 2026
#
# Description:
#   Creates Subject-Level Analysis Dataset (ADSL) with derived variables:
#   - AGEGR9/AGEGR9N: Age groupings (<18, 18-50, >50)
#   - TRTSDTM/TRTSTMF: Treatment start datetime with imputation flag
#   - ITTFL: Intent-to-treat population flag
#   - LSTAVLDT: Last known alive date from multiple sources
#
# Input:  pharmaversesdtm::dm, ex, vs, ae, ds
# Output: adsl.rds, adsl.csv, qc_summary.csv
#
# ==============================================================================

# ── Setup ──────────────────────────────────────────────────────────────────────

# Package management
pkgs <- c("admiral", "dplyr", "stringr", "pharmaversesdtm")
to_install <- pkgs[!pkgs %in% base::rownames(utils::installed.packages())]
if (base::length(to_install) > 0) utils::install.packages(to_install)

base::suppressPackageStartupMessages({
  base::library(admiral)
  base::library(dplyr)
  base::library(stringr)
  base::library(pharmaversesdtm)
})

# Constants
OUTPUT_DIR <- "question_2_adam/output"

COMPLETE_DATE_PATTERN <- "^\\d{4}-\\d{2}-\\d{2}"

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Log file (evidence code runs error-free)
log_file <- base::file.path(OUTPUT_DIR, "run_log.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

# ── Load SDTM Datasets ─────────────────────────────────────────────────────────

base::cat("Loading SDTM datasets...\n")

dm <- admiral::convert_blanks_to_na(pharmaversesdtm::dm)
vs <- admiral::convert_blanks_to_na(pharmaversesdtm::vs)
ex <- admiral::convert_blanks_to_na(pharmaversesdtm::ex)
ds <- admiral::convert_blanks_to_na(pharmaversesdtm::ds)
ae <- admiral::convert_blanks_to_na(pharmaversesdtm::ae)

base::cat("  DM: ", base::nrow(dm), " subjects\n")
base::cat("  EX: ", base::nrow(ex), " records\n")
base::cat("  VS: ", base::nrow(vs), " records\n")
base::cat("  AE: ", base::nrow(ae), " records\n")
base::cat("  DS: ", base::nrow(ds), " records\n\n")

# ── Initialize ADSL from DM ────────────────────────────────────────────────────

base::cat("Initializing ADSL from DM...\n")

adsl <- dm |>
  dplyr::select(-dplyr::any_of("DOMAIN")) |>
  dplyr::mutate(
    TRT01P = ARM,
    TRT01A = ACTARM
  )

base::cat("  ", base::nrow(adsl), " subjects\n\n")

# ── Derive AGEGR9 & AGEGR9N ────────────────────────────────────────────────────

base::cat("Deriving age groupings...\n")

# SPECIFICATION: Age groups - <18 (1), 18-50 (2), >50 (3)
adsl <- adsl |>
  dplyr::mutate(
    AGEGR9N = dplyr::case_when(
      AGE < 18              ~ 1L,
      AGE >= 18 & AGE <= 50 ~ 2L,
      AGE > 50              ~ 3L,
      TRUE                  ~ NA_integer_
    ),
    AGEGR9 = dplyr::case_when(
      AGEGR9N == 1 ~ "<18",
      AGEGR9N == 2 ~ "18 - 50",
      AGEGR9N == 3 ~ ">50",
      TRUE         ~ NA_character_
    )
  )

age_dist <- adsl |>
  dplyr::count(AGEGR9, AGEGR9N) |>
  dplyr::arrange(AGEGR9N)

base::cat("  Age distribution:\n")
base::print(age_dist)
base::cat("\n")

# ── Derive TRTSDTM & TRTSTMF ───────────────────────────────────────────────────

base::cat("Deriving treatment start datetime...\n")

# SPECIFICATION:
# - First exposure with complete date (YYYY-MM-DD) and valid dose
# - Valid dose: EXDOSE > 0 OR (EXDOSE = 0 AND EXTRT contains "PLACEBO")
# - Missing time imputed to 00:00:00
# - TRTSTMF flag for hours/minutes imputation

ex_eligible <- ex |>
  dplyr::filter(
    !base::is.na(EXSTDTC),
    stringr::str_detect(EXSTDTC, COMPLETE_DATE_PATTERN),
    EXDOSE > 0 | (EXDOSE == 0 & stringr::str_detect(toupper(EXTRT), "PLACEBO"))
  ) |>
  admiral::derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    highest_imputation = "M",
    time_imputation = "00:00:00"
  )

ex_first <- ex_eligible |>
  dplyr::arrange(STUDYID, USUBJID, EXSTDTM, EXSEQ) |>
  dplyr::group_by(STUDYID, USUBJID) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::transmute(STUDYID, USUBJID, TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF)

adsl <- adsl |>
  dplyr::left_join(ex_first, by = c("STUDYID", "USUBJID"))

base::cat("  TRTSDTM derived for ", base::sum(!base::is.na(adsl$TRTSDTM)), 
          "/", base::nrow(adsl), " subjects\n\n")

# ── Derive ITTFL ───────────────────────────────────────────────────────────────

base::cat("Deriving ITT flag...\n")

# SPECIFICATION: "Y" if ARM populated (randomized), "N" otherwise
adsl <- adsl |>
  dplyr::mutate(
    ITTFL = dplyr::if_else(!base::is.na(ARM) & base::nchar(ARM) > 0, "Y", "N")
  )

base::cat("  ITT (Y): ", base::sum(adsl$ITTFL == "Y"), " subjects\n\n")

# ── Derive LSTAVLDT ────────────────────────────────────────────────────────────

base::cat("Deriving last known alive date...\n")

# SPECIFICATION: MAX of last dates from VS, AE, DS, EX (all with complete dates)

# VS: Last visit with valid result
vs_last <- vs |>
  dplyr::filter(
    !base::is.na(VSDTC),
    stringr::str_detect(VSDTC, COMPLETE_DATE_PATTERN),
    !(base::is.na(VSSTRESN) & base::is.na(VSSTRESC))
  ) |>
  dplyr::mutate(VSDT = base::as.Date(base::substr(VSDTC, 1, 10))) |>
  dplyr::group_by(STUDYID, USUBJID) |>
  dplyr::summarise(VSLASTDT = base::max(VSDT), .groups = "drop")

# AE: Last AE start date
ae_last <- ae |>
  dplyr::filter(
    !base::is.na(AESTDTC),
    stringr::str_detect(AESTDTC, COMPLETE_DATE_PATTERN)
  ) |>
  dplyr::mutate(AEDT = base::as.Date(base::substr(AESTDTC, 1, 10))) |>
  dplyr::group_by(STUDYID, USUBJID) |>
  dplyr::summarise(AELASTDT = base::max(AEDT), .groups = "drop")

# DS: Last disposition date
ds_last <- ds |>
  dplyr::filter(
    !base::is.na(DSSTDTC),
    stringr::str_detect(DSSTDTC, COMPLETE_DATE_PATTERN)
  ) |>
  dplyr::mutate(DSDT = base::as.Date(base::substr(DSSTDTC, 1, 10))) |>
  dplyr::group_by(STUDYID, USUBJID) |>
  dplyr::summarise(DSLASTDT = base::max(DSDT), .groups = "drop")

# EX: Last exposure end date with valid dose
ex_last <- ex |>
  dplyr::filter(
    !base::is.na(EXENDTC),
    stringr::str_detect(EXENDTC, COMPLETE_DATE_PATTERN),
    EXDOSE > 0 | (EXDOSE == 0 & stringr::str_detect(base::toupper(EXTRT), "PLACEBO"))
  ) |>
  dplyr::mutate(EXDT = base::as.Date(base::substr(EXENDTC, 1, 10))) |>
  dplyr::group_by(STUDYID, USUBJID) |>
  dplyr::summarise(EXLASTDT = base::max(EXDT), .groups = "drop")

# Merge and calculate max
adsl <- adsl |>
  dplyr::left_join(vs_last, by = c("STUDYID", "USUBJID")) |>
  dplyr::left_join(ae_last, by = c("STUDYID", "USUBJID")) |>
  dplyr::left_join(ds_last, by = c("STUDYID", "USUBJID")) |>
  dplyr::left_join(ex_last, by = c("STUDYID", "USUBJID")) |>
  dplyr::mutate(
    LSTAVLDT = base::pmax(VSLASTDT, AELASTDT, DSLASTDT, EXLASTDT, na.rm = TRUE),
    LSTAVLDT = dplyr::if_else(base::is.infinite(LSTAVLDT), base::as.Date(NA), LSTAVLDT)
  ) |>
  dplyr::select(-VSLASTDT, -AELASTDT, -DSLASTDT, -EXLASTDT)

base::cat("  LSTAVLDT derived for ", base::sum(!base::is.na(adsl$LSTAVLDT)), 
          " subjects\n\n")

# ── Quality Control ────────────────────────────────────────────────────────────

base::cat("Running quality control...\n")

qc <- adsl |>
  dplyr::summarise(
    n_subjects = dplyr::n(),
    n_unique_usubjid = dplyr::n_distinct(USUBJID),
    n_na_agegr9 = base::sum(base::is.na(AGEGR9)),
    n_na_trtsdtm = base::sum(base::is.na(TRTSDTM)),
    n_itt = base::sum(ITTFL == "Y"),
    n_na_lstavldt = base::sum(base::is.na(LSTAVLDT))
  )

base::cat("\nQC Summary:\n")
base::print(qc)

# CRITICAL: Verify one record per subject
if (qc$n_subjects != qc$n_unique_usubjid) {
  stop("ERROR: Duplicate USUBJIDs in ADSL")
}

cat("\n✓ One record per subject verified\n\n")

# Save QC summary
write.csv(qc, file.path(OUTPUT_DIR, "qc_summary.csv"), row.names = FALSE)

# ── Save Outputs ───────────────────────────────────────────────────────────────

cat("Saving outputs...\n")

saveRDS(adsl, file.path(OUTPUT_DIR, "adsl.rds"))
write.csv(adsl, file.path(OUTPUT_DIR, "adsl.csv"), row.names = FALSE)
capture.output(sessionInfo(), 
                      file = file.path(OUTPUT_DIR, "session_info.txt"))

base::cat("  ✓ adsl.rds\n")
base::cat("  ✓ adsl.csv\n")
base::cat("  ✓ qc_summary.csv\n")
base::cat("  ✓ session_info.txt\n")

# ── Summary ────────────────────────────────────────────────────────────────────

base::cat("\n", base::paste(base::rep("=", 80), collapse = ""), "\n", sep = "")
base::cat("ADSL CREATION COMPLETED\n")
base::cat(base::paste(base::rep("=", 80), collapse = ""), "\n\n", sep = "")
base::cat("Subjects:        ", base::nrow(adsl), "\n")
base::cat("ITT population:  ", qc$n_itt, " (", 
          base::round(100 * qc$n_itt / base::nrow(adsl), 1), "%)\n")
base::cat("With TRTSDTM:    ", base::nrow(adsl) - qc$n_na_trtsdtm, " (",
          base::round(100 * (base::nrow(adsl) - qc$n_na_trtsdtm) / base::nrow(adsl), 1), "%)\n")
base::cat("With LSTAVLDT:   ", base::nrow(adsl) - qc$n_na_lstavldt, " (",
          base::round(100 * (base::nrow(adsl) - qc$n_na_lstavldt) / base::nrow(adsl), 1), "%)\n")
base::cat("\nOutput: ", OUTPUT_DIR, "\n")
base::cat(base::paste(base::rep("=", 80), collapse = ""), "\n", sep = "")

# End of script