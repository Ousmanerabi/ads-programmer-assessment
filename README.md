# Pharmaverse Expertise & Clinical Reporting Assessment

This repository contains my solutions to the **Pharmaverse Expertise and Coding Assessment** focused on SDTM, ADaM, and clinical reporting using open-source R packages from the Pharmaverse ecosystem.

Questions 1–3 (required) are fully implemented in R.  
Question 4 (Python) is not included.

---

## Repository Structure

```text
ads-programmer-assessment/
├── README.md
├── question_1_sdtm/
│   ├── 01_create_ds_domain.R
│   └── output/
│       ├── ds.rds
│       ├── ds.csv
│       ├── qc_summary.csv
│       ├── run_log.txt
│       └── session_info.txt
├── question_2_adam/
│   ├── create_adsl.R
│   └── output/
│       ├── adsl.rds
│       ├── adsl.csv
│       ├── qc_summary.csv
│       ├── run_log.txt
│       └── session_info.txt
└── question_3_tlg/
    ├── 01_create_ae_summary_table.R
    ├── 02_create_visualizations.R
    ├── logs/
    │   ├── run_log_01_table.txt
    │   └── run_log_02_plots.txt
    └── output/
        ├── ae_summary_table.html
        ├── ae_summary_table.docx
        ├── qc_summary.csv
        ├── f_ae_severity_by_arm.png
        └── f_top10_ae_with_ci.png
```
## Question 1 — SDTM DS Domain ({sdtm.oak})

**Objective**  
Create the SDTM Disposition (DS) domain from raw clinical trial data.

**Key Points**
- Uses `{sdtm.oak}` and study-controlled terminology
- Variables derived per SDTM IG v3.4
- Output includes DS dataset and execution log

**Deliverables**
- `question_1_sdtm/01_create_ds_domain.R`
- DS dataset (any format)
- Log file showing error-free execution

---

## Question 2 — ADaM ADSL Dataset ({admiral})

**Objective**  
Create the Subject-Level Analysis Dataset (ADSL) from SDTM data.

**Input SDTM Domains**
- `pharmaversesdtm::dm`
- `pharmaversesdtm::ex`
- `pharmaversesdtm::vs`
- `pharmaversesdtm::ae`
- `pharmaversesdtm::ds`

**Derived Variables**
- `AGEGR9`, `AGEGR9N`
- `TRTSDTM`, `TRTSTMF`
- `ITTFL`
- `LSTAVLDT`

**Key Points**
- DM used as the base dataset
- `{admiral}` functions used where applicable
- All derivations follow the provided specifications
- One record per subject enforced and QC-checked

**Deliverables**
- `question_2_adam/create_adsl.R`
- ADSL dataset (RDS / CSV)
- QC summary and execution log

---

## Question 3 — TLG: Adverse Events Reporting

**Objective**  
Create regulatory-style adverse event outputs using ADAE and ADSL.

### 3.1 AE Summary Table ({gtsummary})

**Specifications**
- TEAEs identified using `TRTEMFL == "Y"`
- Subject-level incidence
- Rows: `AETERM`
- Columns: `ACTARM`
- Cell values: `n (%)`
- Includes overall column
- Sorted by descending frequency

**Outputs**
- `ae_summary_table.html`
- `ae_summary_table.docx`


---

### 3.2 AE Visualizations ({ggplot2})

**Plot 1 — AE Severity Distribution**
- Event-level counts
- Stratified by treatment (`ACTARM`)
- Variable: `AESEV`

**Plot 2 — Top 10 Most Frequent AEs**
- Subject-level
- Variable: `AETERM`
- 95% confidence intervals (Clopper–Pearson exact method)

**Outputs**
- `f_ae_severity_by_arm.png`
- `f_top10_ae_with_ci.png`

---

## Reproducibility

- All scripts install required packages automatically
- Execution logs are saved for each question
- Session information captured for reproducibility
- Compatible with **R ≥ 4.2.0** and Posit Cloud

---

## Notes

- AI-enabled coding assistants were used in line with assessment guidelines
- Focus was placed on correctness, clarity, reproducibility, and alignment with clinical reporting standards
- Python bonus question (Question 4) was not attempted

---
