# ==============================================================================
# Question 3 - AE Visualizations (TEAE severity + Top10 with 95% CI)
# Program: question_3_tlg/02_create_visualizations.R
#
# Creates:
#   - question_3_tlg/output/f_ae_severity_by_arm.png
#   - question_3_tlg/output/f_top10_ae_with_ci.png
#   - question_3_tlg/output/run_log_02_plots.txt
#
# Input:
#   - pharmaverseadam::adae
#   - pharmaverseadam::adsl
#
# Spec:
#   - TEAE records: TRTEMFL == "Y"
#   - Plot 1: AESEV distribution by ACTARM (bar chart; event-level)
#   - Plot 2: Top 10 AETERM incidence (subject-level; overall denominator) with 95% CI
# ==============================================================================

# Constants

OUTPUT_DIR <- "question_3_tlg/output"
OUTPUT_DIR_log <- "question_3_tlg/logs"

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

dir.create(OUTPUT_DIR_log, recursive = TRUE, showWarnings = FALSE)

# Log file (evidence code runs error-free)
log_file <- base::file.path(OUTPUT_DIR_log, "run_log_02_plots.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

# Installed Packages if not already done before and load them

pkgs <- c("dplyr", "ggplot2", "pharmaverseadam")
to_install <- pkgs[!pkgs %in% rownames(utils::installed.packages())]
if (base::length(to_install) > 0) utils::install.packages(to_install)


base::suppressPackageStartupMessages({
  base::library(dplyr)
  base::library(ggplot2)
  base::library(pharmaverseadam)
})

cat("=== Q3 AE Visualizations ===\n")
cat("Run: ", format(base::Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ---- Load data ----
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

cat("Rows: ADAE=", nrow(adae), " & ADSL=", nrow(adsl), "\n")

# Keep only subjects with a non-missing ACTARM
# IMPORTANT: keep only IDs here to avoid duplicated (ACTARM.x / ACTARM.y) during join
adsl_ids <- adsl %>%
  dplyr::filter(!is.na(ACTARM), ACTARM != "") %>%
  dplyr::distinct(STUDYID, USUBJID)

n_total <- adsl_ids %>% dplyr::distinct(USUBJID) %>% nrow()
cat("Overall denominator (unique USUBJID with ACTARM): N=", n_total, "\n\n")

# TEAE base (event-level): filter ADAE then restrict to ADSL subjects
# Keep ACTARM coming from ADAE (no duplicate join variable -> no .x/.y)
teae_base <- adae %>%
  dplyr::filter(TRTEMFL == "Y") %>%
  dplyr::inner_join(adsl_ids, by = c("STUDYID", "USUBJID"))

cat("TEAE records after filtering and restricting to ADSL subjects: ",
          nrow(teae_base), "\n\n")

# ------------------------------------------------------------------------------
# Plot 1: AESEV distribution by ACTARM (event-level)
# ------------------------------------------------------------------------------

sev_df <- teae_base %>%
  dplyr::filter(!is.na(AESEV), AESEV != "",
                !is.na(ACTARM), ACTARM != "")

base::cat("Plot1 data rows (non-missing AESEV + ACTARM): ", base::nrow(sev_df), "\n")

p1 <- ggplot2::ggplot(sev_df, ggplot2::aes(x = ACTARM, fill = AESEV)) +
  ggplot2::geom_bar(position = "dodge") +
  ggplot2::labs(
    title = "TEAE Severity Distribution by Treatment",
    x = "Treatment (ACTARM)",
    y = "Number of TEAE events",
    fill = "AESEV"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

out_p1 <- file.path(OUTPUT_DIR, "f_ae_severity_by_arm.png")
ggplot2::ggsave(out_p1, p1, width = 10, height = 6, dpi = 300)
base::cat("Saved: ", out_p1, "\n\n")

# ------------------------------------------------------------------------------
# Plot 2: Top 10 AETERM with 95% CI (subject-level incidence; overall denominator)
# ------------------------------------------------------------------------------

# Subject counted once per term overall
teae_term <- teae_base %>%
  dplyr::filter(!is.na(AETERM), AETERM != "") %>%
  dplyr::distinct(USUBJID, AETERM)

cat("Unique AETERM (overall): ", dplyr::n_distinct(teae_term$AETERM), "\n")

top10 <- teae_term %>%
  dplyr::count(AETERM, name = "n") %>%
  dplyr::arrange(dplyr::desc(n), AETERM) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    bt = list(stats::binom.test(n, n_total, conf.level = 0.95)),
    pct = 100 * (n / n_total),
    pct_low = 100 * bt$conf.int[1],
    pct_high = 100 * bt$conf.int[2]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(AETERM = base::factor(AETERM, levels = base::rev(AETERM)))

p2 <- ggplot2::ggplot(top10, ggplot2::aes(y = AETERM, x = pct)) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = pct_low, xmax = pct_high),
    height = 0.25
  ) +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(
    title = "Top 10 TEAE Preferred Terms (Incidence with 95% CI)" ,
    subtitle = base::paste0("n = ", n_total),
    x = "Percentage of patients (%)",
    y = "AETERM"
  ) +
  ggplot2::theme_minimal()

out_p2 <- base::file.path(OUTPUT_DIR, "f_top10_ae_with_ci.png")
ggplot2::ggsave(out_p2, p2, width = 10, height = 6, dpi = 300)
base::cat("Saved: ", out_p2, "\n\n")

base::cat("Done.\n")

# End of the script