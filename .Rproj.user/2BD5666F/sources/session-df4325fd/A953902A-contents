# -------------------------------------------------------------------------
# Question 1: Create SDTM DS (Disposition) domain using {sdtm.oak}
#
# This script:
#   1) Loads raw DS-like source data (pharmaverseraw::ds_raw)
#   2) Creates OAK-standard ID variables (generate_oak_id_vars)
#   3) Maps source columns into SDTM DS variables (assign_no_ct)
#   4) Derives key SDTM variables (e.g., STUDYID, DOMAIN, USUBJID, DSSEQ)
#   5) Writes the resulting DS dataset to RDS/CSV
# -------------------------------------------------------------------------

study_ct <-
  data.frame(
    stringsAsFactors = FALSE,
    codelist_code = c("C66727","C66727",
                      "C66727","C66727","C66727","C66727","C66727","C66727",
                      "C66727","C66727"),
    term_code = c("C41331","C25250",
                  "C28554","C48226","C48227","C48250","C142185","C49628",
                  "C49632","C49634"),
    term_value = c("ADVERSE EVENT",
                   "COMPLETED","DEATH","LACK OF EFFICACY","LOST TO FOLLOW-UP",
                   "PHYSICIAN DECISION","PROTOCOL VIOLATION",
                   "SCREEN FAILURE","STUDY TERMINATED BY SPONSOR",
                   "WITHDRAWAL BY SUBJECT"),
    collected_value = c("Adverse Event",
                        "Complete","Dead","Lack of Efficacy","Lost To Follow-Up",
                        "Physician Decision","Protocol Violation",
                        "Trial Screen Failure","Study Terminated By Sponsor",
                        "Withdrawal by Subject"),
    term_preferred_term = c("AE","Completed","Died",
                            NA,NA,NA,"Violation",
                            "Failure to Meet Inclusion/Exclusion Criteria",NA,"Dropout"),
    term_synonyms = c("ADVERSE EVENT",
                      "COMPLETE","Death",NA,NA,NA,NA,NA,NA,
                      "Discontinued Participation")
  )

library(sdtm.oak)
library(pharmaverseraw)
library(dplyr)

# --- 1) Load raw source data ----------------------------------------------
# pharmaverseraw::ds_raw is an example raw dataset shipped with the package.
ds_raw <- pharmaverseraw::ds_raw

# --- 2) Generate OAK-standard ID variables --------------------------------
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# --- 3) Map raw columns to SDTM variables ---------------------------------
ds <-
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    id_vars = oak_id_vars()
  )

# --- 4) Derive SDTM-standard variables ------------------------------------
ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    DSDECOD = toupper(DSDECOD),
    DSTERM = toupper(DSTERM),
    DSCAT = "DISPOSITION EVENT",
    VISIT    = NA_character_,
    VISITNUM = NA_real_,
    DSDTC = DSSTDTC,
    DSSTDY = NA_real_
  ) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("DSTERM", "DSDECOD", "DSSTDTC")
  ) %>%
  dplyr::select(
    STUDYID, DOMAIN, USUBJID, DSSEQ,
    DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT,
    DSDTC, DSSTDTC, DSSTDY
  )

# --- 5) Sanity check + write outputs --------------------------------------
stopifnot(exists("ds"))

dir.create("question_1_sdtm/output", recursive = TRUE, showWarnings = FALSE)

saveRDS(ds, "question_1_sdtm/output/ds.rds")
write.csv(ds, "question_1_sdtm/output/ds.csv", row.names = FALSE)
message("Saved DS to question_1_sdtm/output/ds.rds and ds.csv")