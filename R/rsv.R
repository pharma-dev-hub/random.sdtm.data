# SV Dataset

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

set.seed(2025)

# Load DM dataset
dm <- read.csv("synthetic_dm.csv", stringsAsFactors = FALSE)
dm <- dm %>% filter(!is.na(RFXSTDTC))
dm$RFXSTDTC <- as.Date(dm$RFXSTDTC)
dm$RFXENDTC <- as.Date(dm$RFXENDTC)

# Load SE dataset
se <- read.csv("synthetic_se.csv", stringsAsFactors = FALSE)
se$SESTDTC <- as.Date(se$SESTDTC)
se$SEENDTC <- as.Date(se$SEENDTC)

# Load TV dataset
tv <- read.csv("synthetic_tv.csv", stringsAsFactors = FALSE)

# Visit structure
visit_template <- tibble(
  VISIT = c("Screening", "Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "Week 10", "Follow-up"),
  VISITNUM = 1:8,
  FIXED_VISITDY = c(NA, 1, 15, 29, 43, 57, 71, 99)
)

# Generate SV dataset
sv <- dm %>%
  select(STUDYID, USUBJID, RFXSTDTC) %>%
  mutate(DOMAIN = "SV") %>%
  crossing(visit_template) %>%
  rowwise() %>%
  mutate(
    RAW_VISITDY = ifelse(VISIT == "Screening", sample(-14:-1, 1), FIXED_VISITDY),
    SVSTDTC = RFXSTDTC + days(RAW_VISITDY - 1),
    SVENDTC = SVSTDTC + days(1),
    SVSTDY = ifelse(SVSTDTC < RFXSTDTC, as.integer(SVSTDTC - RFXSTDTC),
                    as.integer(SVSTDTC - RFXSTDTC) + 1),
    SVENDY = ifelse(SVENDTC < RFXSTDTC, as.integer(SVENDTC - RFXSTDTC),
                    as.integer(SVENDTC - RFXSTDTC) + 1),
    VISITDY = RAW_VISITDY,
  ) %>%
  ungroup() %>%
  arrange(USUBJID, VISITNUM)

# Assign epoch function
sv <- epoch(sv, dtc = "SVSTDTC")

# Join TV metadata
sv <- sv %>%
  left_join(tv %>% select(VISITNUM, VISIT, TVSTRL, TVENRL), by = c("VISITNUM", "VISIT")) %>%
  select(
    STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, EPOCH,
    SVSTDTC, SVENDTC, SVSTDY, SVENDY
  )

# Apply metadata
sv <- apply_metadata(sv, list(
  STUDYID  = "Study Identifier",
  DOMAIN   = "Domain Abbreviation",
  USUBJID  = "Unique Subject Identifier",
  VISITNUM = "Visit Number",
  VISIT    = "Visit Name",
  VISITDY  = "Planned Study Day of Visit",
  EPOCH    = "Epoch",
  SVSTDTC  = "Start Date/Time of Visit",
  SVENDTC  = "End Date/Time of Visit",
  SVSTDY   = "Study Day of Start of Visit",
  SVENDY   = "Study Day of End of Visit"
))

# Output
print(head(sv))
write.csv(sv, "synthetic_sv.csv", row.names = FALSE)


