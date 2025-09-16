set.seed(2025)

# Load DM dataset
dm <- read.csv("synthetic_dm.csv")

library(tibble)
library(lubridate)
library(checkmate)
library(dplyr)
library(tidyr)

# Add BMI and BMI_CAT to DM
dm <- dm %>%
  mutate(
    HEIGHT_CM = runif(n(), 150, 190),
    WEIGHT_KG = runif(n(), 50, 120),
    BMI = round(WEIGHT_KG / (HEIGHT_CM / 100)^2, 1),
    BMI_CAT = as.character(cut(
      BMI,
      breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
      labels = c("UNDERWEIGHT", "NORMAL", "OVERWEIGHT", "OBESE")
    ))
  )

# Create SUPPDM dataset with only BMI_CAT
suppdm <- dm %>%
  select(STUDYID, USUBJID, SUBJID, BMI_CAT) %>%
  mutate(
    QNAM = "BMI_CAT",
    QLABEL = "BMI Category (WHO)",
    QVAL = BMI_CAT,
    RDOMAIN = "DM",
    IDVAR = "SUBJID",
    IDVARVAL = SUBJID,
    QORIG = "DERIVED",
    QEVAL = "SYSTEM"
  ) %>%
  select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL)

# Optional: apply metadata if function is defined
apply_metadata <- function(df, metadata) {
  attr(df, "metadata") <- metadata
  return(df)
}

suppdm <- apply_metadata(suppdm, list(
  STUDYID   = "Study Identifier",
  RDOMAIN   = "Related Domain Abbreviation",
  USUBJID   = "Unique Subject Identifier",
  IDVAR     = "Identifying Variable",
  IDVARVAL  = "Identifying Variable Value",
  QNAM      = "Qualifier Variable Name",
  QLABEL    = "Qualifier Variable Label",
  QVAL      = "Data Value",
  QORIG     = "Origin",
  QEVAL     = "Evaluator"
))

# Output
print(head(suppdm))
write.csv(suppdm, "synthetic_suppdm.csv", row.names = FALSE)


