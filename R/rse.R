library(dplyr)
library(lubridate)

set.seed(2025)

# Load DM dataset
dm <- read.csv("synthetic_dm.csv") %>%
  filter(!is.na(RFXSTDTC) & !is.na(RFXENDTC)) %>%
  mutate(RFXSTDTC = ymd(RFXSTDTC),
         RFXENDTC = ymd(RFXENDTC))

# Generate SE dataset using SAP logic
se <- dm %>%
  rowwise() %>%
  do({
    usubjid <- .$USUBJID
    studyid <- .$STUDYID
    rfxstdtc <- .$RFXSTDTC
    rfxendtc <- .$RFXENDTC

    # Define element periods
    screening_start <- rfxstdtc - days(14)
    screening_end   <- rfxstdtc

    treatment_start <- rfxstdtc
    treatment_end   <- rfxstdtc + days(70)

    followup_start  <- rfxendtc
    followup_end    <- rfxendtc + days(28)

    # Calculate study days relative to RFXSTDTC
    sestdy <- c(-14, 0, as.integer(difftime(followup_start, rfxstdtc, units = "days")))
    seendy <- c(0, 70, as.integer(difftime(followup_end, rfxstdtc, units = "days")))

    data.frame(
      STUDYID  = studyid,
      DOMAIN   = "SE",
      USUBJID  = usubjid,
      SESEQ    = 1:3,
      ETCD     = c("SCR", "TRT", "FUP"),
      ELEMENT  = c("Screening", "Treatment", "Follow-Up"),
      SESTDTC  = format(c(screening_start, treatment_start, followup_start), "%Y-%m-%d"),
      SEENDTC  = format(c(screening_end, treatment_end, followup_end), "%Y-%m-%d"),
      TAETORD  = 1:3,
      EPOCH    = c("Screening", "Treatment", "Follow-Up"),
      SEUPDES  = "",
      SESTDY   = sestdy,
      SEENDY   = seendy
    )
  }) %>%
  ungroup()

# Apply metadata labels
se <- apply_metadata(se, list(
  STUDYID  = "Study Identifier",
  DOMAIN   = "Domain Abbreviation",
  USUBJID  = "Unique Subject Identifier",
  SESEQ    = "Sequence Number",
  ETCD     = "Element Code",
  ELEMENT  = "Description of Element",
  SESTDTC  = "Start Date/Time of Element",
  SEENDTC  = "End Date/Time of Element",
  TAETORD  = "Planned Order of Element within Arm",
  EPOCH    = "Epoch",
  SEUPDES  = "Description of Unplanned Element",
  SESTDY   = "Study Day of Start of Observation",
  SEENDY   = "Study Day of End of Observation"
))

# Ensure SESEQ is correctly mapped using your seqnum() function
se <- seqnum(se, sort = c("USUBJID", "SESTDTC"))

# Optionally re-map EPOCH using your epoch() function (if SESTDTC overlaps with other elements)
# se <- epoch(se, dtc = "SESTDTC")

# Output
print(head(se))
write.csv(se, "synthetic_se.csv", row.names = FALSE)



