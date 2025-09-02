# EX Dataset

library(dplyr)
library(lubridate)
library(readr)

set.seed(2025)

# Load DM dataset
dm <- read_csv("synthetic_dm.csv", show_col_types = FALSE)
dm$RFXSTDTC <- as.Date(dm$RFXSTDTC)
dm$RFXENDTC <- as.Date(dm$RFXENDTC)

# Define dosing schedule: every 2 weeks for 10 weeks
dose_days <- seq(0, 56, by = 14)  # Days from RFXSTDTC

# Define treatment details
treatment_details <- tibble(
  ARMCD = c("DER", "ADA", "PLA"),
  EXTRT = c("Dermavalimab", "Adalimumab", "Placebo"),
  EXDOSE = c(300, 40, 0),
  EXDOSU = "mg",
  EXDOSFRM = "Injection",
  EXROUTE = "SC",
  EXLOC = "Arm",
  EXLAT = "Left",
  EXTRTV = "Vehicle A",
  EXCAT = "IMP"
)

# Generate EX dataset
ex <- dm %>%
  rowwise() %>%
  do({
    subject <- .
    arm <- subject$ACTARMCD
    trt <- filter(treatment_details, ARMCD == arm)

    tibble(
      STUDYID = subject$STUDYID,
      DOMAIN = "EX",
      USUBJID = subject$USUBJID,
      EXSPID = "",  # Optional
      EXTRT = trt$EXTRT,
      EXCAT = trt$EXCAT,
      EXDOSE = trt$EXDOSE,
      EXDOSU = trt$EXDOSU,
      EXDOSFRM = trt$EXDOSFRM,
      EXROUTE = trt$EXROUTE,
      EXLOC = trt$EXLOC,
      EXLAT = trt$EXLAT,
      EXTRTV = trt$EXTRTV,
      VISITNUM = seq_along(dose_days),
      VISIT = paste("Week", dose_days / 7),
      VISITDY = dose_days,
      EXSTDTC = format(subject$RFXSTDTC + days(dose_days), "%Y-%m-%d"),
      EXENDTC = format(subject$RFXSTDTC + days(dose_days), "%Y-%m-%d"),
      EXSTDY = dose_days + 1,
      EXENDY = dose_days + 1
    )
  }) %>%
  ungroup()

# Apply epoch function
ex <- epoch(ex, dtc = "EXSTDTC")

# Apply sequence function for exseq
ex <- seqnum(ex, sort = c("USUBJID", "EXSTDTC"))

ex <- ex %>%
  select(
    STUDYID, DOMAIN, USUBJID, EXSEQ, EXSPID, EXTRT, EXCAT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE,
    EXLOC, EXLAT, EXTRTV, VISITNUM, VISIT, VISITDY, EPOCH, EXSTDTC, EXENDTC, EXSTDY, EXENDY,
  )

# Write ex metadata
ex_metadata <- list(
  STUDYID   = "Study Identifier",
  DOMAIN    = "Domain Abbreviation",
  USUBJID   = "Unique Subject Identifier",
  EXSEQ     = "Sequence Number",
  EXTRT     = "Name of Actual Treatment",
  EXDOSE    = "Dose per Administration",
  EXDOSU    = "Dose Units",
  EXDOSFRM  = "Dose Form",
  EXROUTE   = "Route of Administration",
  EXSTDTC   = "Start Date/Time of Treatment",
  EXENDTC   = "End Date/Time of Treatment",
  EXSTDY    = "Study Day of Start of Treatment",
  EXENDY    = "Study Day of End of Treatment",
  VISIT     = "Visit Name",
  VISITDY   = "Planned Study Day of Visit",
  EPOCH     = "Epoch"
)

ex <- apply_metadata(ex, ex_metadata)


# Output
print(head(ex))
write_csv(ex, "synthetic_ex.csv")
