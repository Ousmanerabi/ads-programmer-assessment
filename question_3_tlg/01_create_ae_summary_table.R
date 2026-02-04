# ==============================================================================
# Question 3 - AE Summary Table (TEAE) using {gtsummary}
# Program: question_3_tlg/01_create_ae_summary_table.R
# Creates: ae_summary_table.html, ae_summary_table.docx, ae_summary_table.pdf
# ==============================================================================

OUTPUT_DIR <- "question_3_tlg/output"

OUTPUT_DIR_log <- "question_3_tlg/logs"

base::dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Log file (evidence code runs error-free)
log_file <- base::file.path(OUTPUT_DIR_log, "run_log_01_table.txt")
base::sink(log_file, split = TRUE)
base::on.exit(base::sink(), add = TRUE)

# ---- Packages ----
pkgs <- c("dplyr", "stringr", "gtsummary", "pharmaverseadam", "gt", "flextable", 
          "officer", "pagedown")

to_install <- pkgs[!pkgs %in% base::rownames(utils::installed.packages())]
if (base::length(to_install) > 0) utils::install.packages(to_install)

base::suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(gtsummary)
  library(pharmaverseadam)
  library(gt)
  library(flextable)
  library(officer)
  library(pagedown)
})

cat("=== Q3 AE Summary Table (TEAE) ===\n")
cat("Run: ", format(base::Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ---- Load data ----
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# ---- Keep subjects with treatment group (ACTARM) ----
adsl_arm <- adsl %>%
  dplyr::filter(!is.na(ACTARM), ACTARM != "") %>%
  dplyr::select(STUDYID, USUBJID, ACTARM) %>%
  dplyr::distinct()

cat("Subjects with ACTARM: ", nrow(adsl_arm), "\n")

# ---- TEAE subject-level incidence by AETERM ----
# Spec: TEAE records have TRTEMFL == "Y"
# Spec: subject counted once per term
teae_term <- adae %>%
  dplyr::filter(TRTEMFL == "Y") %>%
  dplyr::select(STUDYID, USUBJID, AETERM) %>%
  dplyr::inner_join(adsl_arm, by = c("STUDYID", "USUBJID")) %>%
  dplyr::filter(!is.na(AETERM), AETERM != "") %>%
  dplyr::distinct(STUDYID, USUBJID, ACTARM, AETERM)

cat("TEAE subject-term rows: ", nrow(teae_term), "\n")

# Order terms by descending total frequency (across all arms)
term_order <- teae_term %>%
  dplyr::distinct(USUBJID, AETERM) %>%
  dplyr::count(AETERM, name = "n_total") %>%
  dplyr::arrange(dplyr::desc(n_total), AETERM) %>%
  dplyr::pull(AETERM)

teae_term <- teae_term %>%
  dplyr::mutate(AETERM = factor(AETERM, levels = term_order))

# ---- Create table using gtsummary ----
tbl_ae <- teae_term %>%
  dplyr::select(ACTARM, AETERM) %>%
  gtsummary::tbl_summary(
    by = ACTARM,
    statistic = gtsummary::all_categorical() ~ "{n} ({p}%)",
    percent = "column",
    missing = "no"
  ) %>%
  gtsummary::add_overall(last = TRUE) %>%
  gtsummary::modify_header(label ~ "**AETERM**") %>%
  gtsummary::bold_labels()

stopifnot(inherits(tbl_ae, "gtsummary"))

# ---- Save HTML (required) ----
out_html <- file.path(OUTPUT_DIR, "ae_summary_table.html")
gt_obj <- gtsummary::as_gt(tbl_ae)
gt::gtsave(gt_obj, out_html)
cat("Saved: ", out_html, "\n")

# ---- Save DOCX (add) ----
out_docx <- file.path(OUTPUT_DIR, "ae_summary_table.docx")
ft_obj <- gtsummary::as_flex_table(tbl_ae)
flextable::save_as_docx(ft_obj, path = out_docx)
cat("Saved: ", out_docx, "\n")

# End of the script
