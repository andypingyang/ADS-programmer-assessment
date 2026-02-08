library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(stringr)
library(gtsummary)
library(gt)

library(pharmaverseadam)

# --- Input ADaM datasets ---------------------------------------------------
adsl <- pharmaverseadam::adsl %>%
  mutate(ACTARM = convert_blanks_to_na(ACTARM))

# choose rows as AESOC
adae <- pharmaverseadam::adae %>%
  mutate(
    ACTARM = convert_blanks_to_na(ACTARM),
    TRTEMFL = convert_blanks_to_na(TRTEMFL),
    AESOC = convert_blanks_to_na(AESOC)
  )

adsl_denoms <- adsl %>%
  filter(SAFFL == "Y") %>%
  distinct(USUBJID, ACTARM)

# --- Create subject-level TEAE-by-AESOC presence --------------------------
teae_aesoc <- adae %>%
  filter(TRTEMFL == "Y") %>%
  filter(!is.na(AESOC), !is.na(ACTARM)) %>%
  distinct(USUBJID, ACTARM, AESOC)

# Pivot to wide: each AESOC becomes a 0/1 column indicating TEAE presence.
subj_aesoc_wide <- teae_aesoc %>%
  mutate(flag = 1L) %>%
  pivot_wider(
    id_cols = c(USUBJID, ACTARM),
    names_from = AESOC,
    values_from = flag,
    values_fill = 0L
  )

subj_all <- adsl_denoms %>%
  left_join(subj_aesoc_wide, by = c("USUBJID", "ACTARM")) %>%
  mutate(across(-c(USUBJID, ACTARM), ~ replace_na(.x, 0L)))

aesoc_vars <- setdiff(names(subj_all), c("USUBJID", "ACTARM"))

# Order AESOCs by overall frequency (descending), so the table is easier to scan
aesoc_order <- subj_all %>%
  summarise(across(all_of(aesoc_vars), ~ sum(.x == 1L))) %>%
  pivot_longer(everything(), names_to = "AESOC", values_to = "n") %>%
  arrange(desc(n)) %>%
  pull(AESOC)

subj_all <- subj_all %>%
  select(USUBJID, ACTARM, all_of(aesoc_order))

# --- Build the table ------------------------------------------------------
# Each AESOC column is treated as a dichotomous variable (0/1), summarized by ACTARM.
tbl <- subj_all %>%
  tbl_summary(
    by = ACTARM,
    include = all_of(aesoc_order),
    type = all_dichotomous() ~ "dichotomous",
    statistic = all_dichotomous() ~ "{n} ({p}%)",
    digits = all_dichotomous() ~ c(0, 1),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  modify_header(
    label ~ "System Organ Class (AESOC)"
  ) %>%
  bold_labels()

# --- Output ---------------------------------------------------------------
dir.create("question_3_tlg/output", recursive = TRUE, showWarnings = FALSE)

gt_tbl <- as_gt(tbl)

gt::gtsave(
  data = gt_tbl,
  filename = "question_3_tlg/output/ae_summary_table_by_aesoc.html"
)

message("Saved AE summary table (by AESOC) to question_3_tlg/output/ae_summary_table_by_aesoc.html")