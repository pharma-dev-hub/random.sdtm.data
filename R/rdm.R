# R/rdm.R
#' Create Random Demographics (DM) Dataset
#'
#' @description
#' Creates a random SDTM DM (Demographics) dataset following CDISC SDTM standards.
#' One record per subject.
#'
#' @param n_patients Number of patients to generate
#' @param studyid Study identifier (default: "STUDY001")
#' @param sites Character vector of site IDs
#' @param investigators List with site IDs as names and investigator info as values
#' @param arms List with arm codes and descriptions
#' @param seed Random seed for reproducibility
#' @param na_prob Probability of NA values for optional fields
#' @param cached Logical, use cached data if available
#'
#' @return A data.frame with SDTM DM structure
#' @export
#'
#' @examples
#' dm <- rdm(n_patients = 100)
#' head(dm)


ta_arms <- ta %>%
  select(ARMCD, ARM) %>%
  distinct() %>%
  arrange(ARMCD)

arms_list <- map2(ta_arms$ARMCD, ta_arms$ARM, ~ list(code = .x, description = .y))

rdm <- function(n_patients = 400,
                sites = c("001", "002", "003", "004"),
                investigators = list(
                  "001" = list(id = "INV001", name = "Dr. Smith, John"),
                  "002" = list(id = "INV002", name = "Dr. Johnson, Mary"),
                  "003" = list(id = "INV003", name = "Dr. Williams, Robert"),
                  "004" = list(id = "INV004", name = "Dr. Brown, Sarah")
                ),
                arms = arms_list,
                study_start = lubridate::ymd("2024-01-01"),
                status_probs = c(ongoing = 0.80, completed = 0.15, discontinued = 0.05),
                study_duration_days = 84,
                sex_probs = c(M = 0.48, F = 0.52),
                race_probs = c(
                  "WHITE" = 0.70,
                  "BLACK OR AFRICAN AMERICAN" = 0.15,
                  "ASIAN" = 0.10,
                  "AMERICAN INDIAN OR ALASKA NATIVE" = 0.03,
                  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = 0.02
                ),
                ethnic_probs = c(
                  "HISPANIC OR LATINO" = 0.15,
                  "NOT HISPANIC OR LATINO" = 0.80,
                  "NA" = 0.05
                ),
                seed = NULL,
                na_prob = 0.05,
                cached = FALSE) {

  # Validate inputs
  checkmate::assert_count(n_patients, positive = TRUE)
  checkmate::assert_string(studyid)
  checkmate::assert_character(sites, min.len = 1)
  checkmate::assert_list(investigators)
  checkmate::assert_list(arms, min.len = 1)
  checkmate::assert_date(study_start)
  checkmate::assert_numeric(status_probs, lower = 0, upper = 1, len = 3)
  checkmate::assert_number(study_duration_days, lower = 1)
  checkmate::assert_numeric(sex_probs, lower = 0, upper = 1, len = 2)
  checkmate::assert_numeric(race_probs, lower = 0, upper = 1)
  checkmate::assert_numeric(ethnic_probs, lower = 0, upper = 1)
  checkmate::assert_number(seed, null.ok = TRUE)
  checkmate::assert_number(na_prob, lower = 0, upper = 1)
  checkmate::assert_flag(cached)

  # Normalize probabilities
  status_probs <- status_probs / sum(status_probs)
  sex_probs <- sex_probs / sum(sex_probs)
  race_probs <- race_probs / sum(race_probs)
  ethnic_probs <- ethnic_probs / sum(ethnic_probs, na.rm = TRUE)

  if (cached) {
    return(get_cached_data("dm"))
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate base demographics
  dm <- tibble::tibble(
    STUDYID = studyid,
    DOMAIN = "DM",
    USUBJID = paste0(studyid, "-", sprintf("%04d", seq_len(n_patients))),
    SUBJID = sprintf("%04d", seq_len(n_patients))
  )

  # Informed consent dates
  dm$RFICDTC <- as.character(study_start - lubridate::days(sample(0:30, n_patients, replace = TRUE)))
  dm$RFSTDTC <- as.character(lubridate::ymd(dm$RFICDTC) + lubridate::days(sample(1:7, n_patients, replace = TRUE)))
  dm$RFXSTDTC <- dm$RFSTDTC

  # End dates
  dm$RFENDTC <- NA_character_
  dm$RFXENDTC <- NA_character_
  dm$RFPENDTC <- NA_character_

  status <- sample(names(status_probs), n_patients, replace = TRUE, prob = status_probs)
  for (i in seq_len(n_patients)) {
    if (status[i] == "completed") {
      end_date <- lubridate::ymd(dm$RFSTDTC[i]) + lubridate::days(study_duration_days)
      dm$RFENDTC[i] <- as.character(end_date)
      dm$RFXENDTC[i] <- as.character(end_date)
      dm$RFPENDTC[i] <- as.character(end_date + lubridate::days(sample(0:7, 1)))
    } else if (status[i] == "discontinued") {
      end_date <- lubridate::ymd(dm$RFSTDTC[i]) + lubridate::days(sample(1:60, 1))
      dm$RFENDTC[i] <- as.character(end_date)
      dm$RFXENDTC[i] <- as.character(end_date)
      dm$RFPENDTC[i] <- as.character(end_date)
    }
  }

  # Death info
  dm$DTHDTC <- NA_character_
  dm$DTHFL <- NA_character_
  death_idx <- sample(seq_len(n_patients), size = ceiling(n_patients * 0.005))
  for (idx in death_idx) {
    if (!is.na(dm$RFENDTC[idx])) {
      dm$DTHDTC[idx] <- dm$RFENDTC[idx]
      dm$DTHFL[idx] <- "Y"
    }
  }

  # Site and investigator
  dm$SITEID <- sample(sites, n_patients, replace = TRUE)
  dm$INVID <- sapply(dm$SITEID, function(site) investigators[[site]]$id)
  dm$INVNAM <- sapply(dm$SITEID, function(site) investigators[[site]]$name)

  # Age and birth date
  dm$AGE <- round(rnorm(n_patients, mean = 55, sd = 12))
  dm$AGE[dm$AGE < 18] <- 18
  dm$AGE[dm$AGE > 75] <- 75
  dm$AGEU <- "YEARS"
  dm$BRTHDTC <- as.character(lubridate::ymd(dm$RFICDTC) - lubridate::years(dm$AGE))

  # Sex, race, ethnicity
  dm$SEX <- sample(names(sex_probs), n_patients, replace = TRUE, prob = sex_probs)
  dm$RACE <- sample(names(race_probs), n_patients, replace = TRUE, prob = race_probs)
  dm$ETHNIC <- sample(names(ethnic_probs), n_patients, replace = TRUE, prob = ethnic_probs)

  # Treatment arms
  arm_codes <- sapply(arms, function(x) x$code)
  dm$ARMCD <- rep(arm_codes, length.out = n_patients)
  dm$ARM <- sapply(dm$ARMCD, function(code) {
    arms[[which(arm_codes == code)]]$description
  })

  # Actual arm (crossover)
  dm$ACTARMCD <- dm$ARMCD
  dm$ACTARM <- dm$ARM
  crossover_idx <- sample(seq_len(n_patients), size = ceiling(n_patients * 0.02))
  for (idx in crossover_idx) {
    new_arm <- sample(arm_codes[arm_codes != dm$ARMCD[idx]], 1)
    dm$ACTARMCD[idx] <- new_arm
    dm$ACTARM[idx] <- arms[[which(arm_codes == new_arm)]]$description
  }

  # Country
  dm$COUNTRY <- sample(
    c("USA", "CAN", "GBR", "DEU", "FRA", "JPN", "AUS"),
    n_patients,
    replace = TRUE,
    prob = c(0.40, 0.15, 0.10, 0.10, 0.10, 0.10, 0.05)
  )

  # Demographics collection date
  dm$DMDTC <- dm$RFICDTC
  dm$DMDY <- as.integer(lubridate::ymd(dm$DMDTC) - lubridate::ymd(dm$RFSTDTC) + 1)

  # Add ARMNRS and ACTARMUD for Pinnacle 21 compliance
  dm$ARMNRS <- dm$ARM
  dm$ACTARMUD <- dm$ACTARM

  # Reorder columns
  dm <- dm %>%
    select(
      STUDYID, DOMAIN, USUBJID, SUBJID,
      RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC, RFICDTC, RFPENDTC,
      DTHDTC, DTHFL, SITEID, INVID, INVNAM, BRTHDTC,
      AGE, AGEU, SEX, RACE, ETHNIC,
      ARMCD, ARM, ARMNRS, ACTARMCD, ACTARM, ACTARMUD, COUNTRY,
      DMDTC, DMDY
    )

  # Apply metadata
  metadata <- list(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    SUBJID   = "Subject Identifier for the Study",
    RFSTDTC  = "Subject Reference Start Date/Time",
    RFENDTC  = "Subject Reference End Date/Time",
    RFXSTDTC = "Date/Time of First Study Treatment",
    RFXENDTC = "Date/Time of Last Study Treatment",
    RFICDTC  = "Date/Time of Informed Consent",
    RFPENDTC = "Date/Time of End of Participation",
    DTHDTC   = "Date/Time of Death",
    DTHFL    = "Subject Death Flag",
    SITEID   = "Study Site Identifier",
    INVID    = "Investigator Identifier",
    INVNAM   = "Investigator Name",
    BRTHDTC  = "Date/Time of Birth",
    AGE      = "Age",
    AGEU     = "Age Units",
    SEX      = "Sex",
    RACE     = "Race",
    ETHNIC   = "Ethnicity",
    ARMCD    = "Planned Arm Code",
    ARM      = "Description of Planned Arm",
    ARMNRS   = "Planned Arm Name for Randomization Stratification",
    ACTARMCD = "Actual Arm Code",
    ACTARM   = "Description of Actual Arm",
    ACTARMUD = "Actual Arm Used",
    COUNTRY  = "Country",
    DMDTC    = "Date/Time of Collection",
    DMDY     = "Study Day of Collection"
  )
  apply_metadata(dm, metadata)

  return(dm)
}

dm <- rdm(n_patients = 79)


