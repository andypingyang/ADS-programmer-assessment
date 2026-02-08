library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(stringr)
library(ggplot2)
library(pharmaverseadam)

# Pull example ADaM datasets and normalize blanks -> NA for cleaner filtering/grouping
adsl <- pharmaverseadam::adsl %>%
  mutate(ACTARM = convert_blanks_to_na(ACTARM))

adae <- pharmaverseadam::adae %>%
  mutate(
    ACTARM = convert_blanks_to_na(ACTARM),
    TRTEMFL = convert_blanks_to_na(TRTEMFL),
    AESEV = convert_blanks_to_na(AESEV),
    AETERM = convert_blanks_to_na(AETERM)
  )

# Output folder for plots
dir.create("question_3_tlg/output", recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# Plot 1: TEAE severity distribution by treatment (record-level)
#
# Key logic:
# - Filter to TEAEs only (TRTEMFL == "Y")
# - Count AE *records* by ACTARM x AESEV
# - Compute within-arm % (pct) for optional labeling or QA
# -------------------------------------------------------------------------
sev_df <- adae %>%
  filter(TRTEMFL == "Y") %>%
  filter(!is.na(ACTARM), !is.na(AESEV)) %>%
  count(ACTARM, AESEV, name = "n") %>%
  group_by(ACTARM) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

p1 <- ggplot(sev_df, aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_col(position = "stack") +
  labs(
    title = "TEAE Severity Distribution by Treatment",
    x = "Treatment (ACTARM)",
    y = "Count of TEAE records",
    fill = "AESEV"
  ) +
  theme_minimal()

ggsave(
  filename = "question_3_tlg/output/plot1_ae_severity_by_treatment.png",
  plot = p1,
  width = 9,
  height = 5,
  dpi = 300
)

message("Saved Plot 1 to question_3_tlg/output/plot1_ae_severity_by_treatment.png")

# -------------------------------------------------------------------------
# Plot 2: Top 10 TEAE terms with 95% CI (SUBJECT incidence, overall)
#
# Key logic:
# - Denominator (n_total): distinct subjects in ADSL (overall)
# - Numerator for each AETERM: distinct subjects with ≥1 TEAE of that term
#   (distinct(USUBJID, AETERM) ensures a subject counts once per term)
# - Compute incidence proportion and 95% CI using prop.test (Wald/score-based)
# - Plot % with error bars; coord_flip() for readability
# -------------------------------------------------------------------------

# Overall denominator: number of unique subjects
n_total <- adsl %>%
  filter(!is.na(USUBJID)) %>%
  distinct(USUBJID) %>%
  nrow()

# Subject-level TEAE presence per term (one row per subject-term)
term_subj <- adae %>%
  filter(TRTEMFL == "Y") %>%
  filter(!is.na(USUBJID), !is.na(AETERM)) %>%
  distinct(USUBJID, AETERM)

# Top 10 terms by number of subjects affected
term_counts <- term_subj %>%
  count(AETERM, name = "n_subj") %>%
  mutate(
    prop = n_subj / n_total
  ) %>%
  arrange(desc(n_subj)) %>%
  slice_head(n = 10)

# 95% CI for each term’s subject incidence (prop.test returns CI on [0,1] scale)
ci_df <- term_counts %>%
  rowwise() %>%
  mutate(
    ci = list(prop.test(x = n_subj, n = n_total, correct = FALSE)$conf.int),
    lcl = ci[[1]][1],
    ucl = ci[[1]][2]
  ) %>%
  ungroup() %>%
  mutate(
    pct = 100 * prop,
    lcl_pct = 100 * lcl,
    ucl_pct = 100 * ucl
  ) %>%
  # Order terms so the plot reads bottom->top from low to high incidence
  arrange(pct) %>%
  mutate(AETERM = factor(AETERM, levels = AETERM))

p2 <- ggplot(ci_df, aes(x = AETERM, y = pct)) +
  geom_point() +
  geom_errorbar(aes(ymin = lcl_pct, ymax = ucl_pct), width = 0.2) +
  coord_flip() +
  labs(
    title = "Top 10 TEAE Terms: Subject Incidence with 95% CI (Overall)",
    x = "AETERM",
    y = "Incidence (% of subjects)"
  ) +
  theme_minimal()

ggsave(
  filename = "question_3_tlg/output/plot2_top10_aeterm_incidence_ci.png",
  plot = p2,
  width = 9,
  height = 6,
  dpi = 300
)
message("Saved Plot 2 to question_3_tlg/output/plot2_top10_aeterm_incidence_ci.png")