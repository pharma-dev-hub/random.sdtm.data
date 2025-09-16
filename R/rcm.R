# CM Dataset

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

set.seed(2025)

# Load DM and SE datasets
dm <- read.csv("synthetic_dm.csv", stringsAsFactors = FALSE)
se <- read.csv("synthetic_se.csv", stringsAsFactors = FALSE)
dm$RFSTDTC <- as.Date(dm$RFSTDTC)
se$SESTDTC <- as.Date(se$SESTDTC)
se$SEENDTC <- as.Date(se$SEENDTC)

# Sample CM terms and hierarchy
cm_terms <- tibble(
  CMTRT = c("Paracetamol", "Ibuprofen", "Amoxicillin", "Cetirizine", "Metformin"),
  CMDECOD = toupper(CMTRT),
  CMCAT = c("Pain relief", "Inflammation", "Infection", "Allergy", "Diabetes")
)

# Select 30% of subjects to have CM records
cm_subjects <- dm %>% sample_frac(0.3)

# Generate CM records
cm <- cm_subjects %>%
  slice(rep(1:n(), each = sample(1:2, 1))) %>%
  group_by(USUBJID) %>%
  mutate(
    CMSPID = NA,
    TERM_INDEX = sample(1:nrow(cm_terms), n(), replace = TRUE),
    CMTRT = cm_terms$CMTRT[TERM_INDEX],
    CMMODIFY = CMTRT,
    CMDECOD = cm_terms$CMDECOD[TERM_INDEX],
    CMCAT = cm_terms$CMCAT[TERM_INDEX],
    CMINDC = sample(c("Headache", "Pain", "Infection", "Allergy", "Diabetes"), n(), replace = TRUE),
    CMDOSE = sample(c(500, 1000, 250), n(), replace = TRUE),
    CMDOSU = sample(c("mg", "ml", "tablet"), n(), replace = TRUE),
    CMDOSFRM = sample(c("Tablet", "Capsule", "Liquid"), n(), replace = TRUE),
    CMDOSFRQ = sample(c("BID", "QD", "PRN"), n(), replace = TRUE),
    CMROUTE = sample(c("Oral", "Topical", "SC"), n(), replace = TRUE),
    CMSTDTC = RFSTDTC + sample(0:60, n(), replace = TRUE),
    CMENDTC = CMSTDTC + sample(5:30, n(), replace = TRUE),
    CMSTDY = as.integer(CMSTDTC - RFSTDTC + 1),
    CMENDY = as.integer(CMENDTC - RFSTDTC + 1),
    CMENRF = "AFTER",
    CMENRTPT = "TREATMENT",
    CMENTPT = "END OF TREATMENT",
    DOMAIN = "CM",
    STUDYID = STUDYID[1]
  ) %>%
  rowwise() %>%
  ungroup()

# Assigning epoch function
cm <- epoch(cm, dtc = "CMSTDTC")

# Assigning sequence function for CMSEQ
cm <- seqnum(cm, sort = c("USUBJID", "CMSTDTC"))

# Apply SDTM length constraints
cm <- cm %>%
  mutate(
    STUDYID = substr(STUDYID, 1, 6),
    DOMAIN = substr(DOMAIN, 1, 2),
    USUBJID = substr(USUBJID, 1, 16),
    CMSPID = substr(CMSPID, 1, 2),
    CMTRT = substr(CMTRT, 1, 200),
    CMMODIFY = substr(CMMODIFY, 1, 200),
    CMDECOD = substr(CMDECOD, 1, 100),
    CMCAT = substr(CMCAT, 1, 200),
    CMINDC = substr(CMINDC, 1, 200),
    CMDOSU = substr(CMDOSU, 1, 20),
    CMDOSFRM = substr(CMDOSFRM, 1, 20),
    CMDOSFRQ = substr(CMDOSFRQ, 1, 20),
    CMROUTE = substr(CMROUTE, 1, 20),
    EPOCH = substr(EPOCH, 1, 9)
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, CMSEQ, CMSPID, CMTRT, CMMODIFY, CMDECOD, CMCAT,
    CMINDC, CMDOSE, CMDOSU, CMDOSFRM, CMDOSFRQ, CMROUTE, EPOCH,
    CMSTDTC, CMENDTC, CMSTDY, CMENDY, CMENRF, CMENRTPT, CMENTPT
  )

cm <- apply_metadata(cm, list(
  STUDYID  = "Study Identifier",
  DOMAIN   = "Domain Abbreviation",
  USUBJID  = "Unique Subject Identifier",
  CMSEQ    = "Sequence Number",
  CMSPID   = "Sponsor Defined Identifier",
  CMTRT    = "Reported Name of Drug, Med or Therapy",
  CMMODIFY = "Modified Report Name",
  CMDECOD  = "Standardized Medication Name",
  CMCAT    = "Category for Medication",
  CMINDC   = "Indication",
  CMDOSE   = "Dose per Administration",
  CMDOSU   = "Dose Units",
  CMDOSFRM = "Dose Form",
  CMDOSFRQ = "Dose Frequency per Interval",
  CMROUTE  = "Route of Administration",
  EPOCH    = "Epoch",
  CMSTDTC  = "Start Date/Time of Medication",
  CMENDTC  = "End Date/Time of Medication",
  CMSTDY   = "Study Day of Start of Medication",
  CMENDY   = "Study Day of End of Medication",
  CMENRF   = "End Relative to Reference Period",
  CMENRTPT = "End Relative to Reference Time Point",
  CMENTPT  = "End Reference Time Point"
))

# View and save
print(head(cm))
write.csv(cm, "synthetic_cm.csv", row.names = FALSE)
