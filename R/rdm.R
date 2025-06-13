# R/rdm.R
#' Create Random Demographics (DM) Dataset
#'
#' @description
#' Creates a random SDTM DM (Demographics) dataset following CDISC SDTM standards.
#' One record per subject.
#'
#' @param n_patients Number of patients to generate
#' @param study_id Study identifier (default: "STUDY001")
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
rdm <- function(n_patients = 400,
                study_id = "STUDY001",
                sites = c("001", "002", "003", "004"),
                investigators = list(
                  "001" = list(id = "INV001", name = "Dr. Smith, John"),
                  "002" = list(id = "INV002", name = "Dr. Johnson, Mary"),
                  "003" = list(id = "INV003", name = "Dr. Williams, Robert"),
                  "004" = list(id = "INV004", name = "Dr. Brown, Sarah")
                ),
                arms = list(
                  list(code = "PBO", description = "Placebo"),
                  list(code = "TRT01", description = "Treatment 50mg"),
                  list(code = "TRT02", description = "Treatment 100mg")
                ),
                seed = NULL,
                na_prob = 0.05,
                cached = FALSE) {

  checkmate::assert_count(n_patients, positive = TRUE)
  checkmate::assert_string(study_id)
  checkmate::assert_character(sites, min.len = 1)
  checkmate::assert_list(investigators)
  checkmate::assert_list(arms, min.len = 1)
  checkmate::assert_number(seed, null.ok = TRUE)
  checkmate::assert_number(na_prob, lower = 0, upper = 1)
  checkmate::assert_flag(cached)

  if (cached) {
    return(get_cached_data("dm"))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate base demographics
  dm <- tibble::tibble(
    STUDYID = study_id,
    DOMAIN = "DM",
    USUBJID = paste0(study_id, "-", sprintf("%04d", seq_len(n_patients))),
    SUBJID = sprintf("%04d", seq_len(n_patients))
  )

  # Generate dates
  # Study start date
  study_start <- lubridate::ymd("2024-01-01")

  # Informed consent dates (0-30 days before reference start)
  dm$RFICDTC <- as.character(study_start - lubridate::days(sample(0:30, n_patients, replace = TRUE)))

  # Reference start date (first exposure to study treatment) - usually 1-7 days after consent
  dm$RFSTDTC <- as.character(lubridate::ymd(dm$RFICDTC) + lubridate::days(sample(1:7, n_patients, replace = TRUE)))

  # First and last study treatment (same as reference for ongoing study)
  dm$RFXSTDTC <- dm$RFSTDTC

  # Generate end dates for some subjects (dropout/completion)
  dm$RFENDTC <- NA_character_
  dm$RFXENDTC <- NA_character_
  dm$RFPENDTC <- NA_character_

  # Simulate 80% still ongoing, 15% completed, 5% discontinued
  status <- sample(c("ongoing", "completed", "discontinued"), n_patients,
                   replace = TRUE, prob = c(0.80, 0.15, 0.05))

  for (i in 1:n_patients) {
    if (status[i] == "completed") {
      # Study duration 12 weeks (84 days)
      end_date <- lubridate::ymd(dm$RFSTDTC[i]) + lubridate::days(84)
      dm$RFENDTC[i] <- as.character(end_date)
      dm$RFXENDTC[i] <- as.character(end_date)
      dm$RFPENDTC[i] <- as.character(end_date + lubridate::days(sample(0:7, 1))) # Follow-up
    } else if (status[i] == "discontinued") {
      # Random discontinuation between 1-60 days
      end_date <- lubridate::ymd(dm$RFSTDTC[i]) + lubridate::days(sample(1:60, 1))
      dm$RFENDTC[i] <- as.character(end_date)
      dm$RFXENDTC[i] <- as.character(end_date)
      dm$RFPENDTC[i] <- as.character(end_date)
    }
  }

  # Death information (very rare - 0.5%)
  dm$DTHDTC <- NA_character_
  dm$DTHFL <- NA_character_
  death_idx <- sample(1:n_patients, size = ceiling(n_patients * 0.005))
  for (idx in death_idx) {
    if (!is.na(dm$RFENDTC[idx])) {
      dm$DTHDTC[idx] <- dm$RFENDTC[idx]
      dm$DTHFL[idx] <- "Y"
    }
  }

  # Site and investigator information
  dm$SITEID <- sample(sites, n_patients, replace = TRUE)
  dm$INVID <- sapply(dm$SITEID, function(site) investigators[[site]]$id)
  dm$INVNAM <- sapply(dm$SITEID, function(site) investigators[[site]]$name)

  # Birth dates and age calculation
  # Generate ages first
  dm$AGE <- round(rnorm(n_patients, mean = 55, sd = 12))
  dm$AGE[dm$AGE < 18] <- 18
  dm$AGE[dm$AGE > 85] <- 85
  dm$AGEU <- "YEARS"

  # Calculate birth dates from age and informed consent date
  dm$BRTHDTC <- as.character(lubridate::ymd(dm$RFICDTC) - lubridate::years(dm$AGE))

  # Demographics
  dm$SEX <- sample(c("M", "F"), n_patients, replace = TRUE, prob = c(0.48, 0.52))

  # Race with proper SDTM controlled terminology
  dm$RACE <- sample(
    c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN",
      "AMERICAN INDIAN OR ALASKA NATIVE",
      "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"),
    n_patients,
    replace = TRUE,
    prob = c(0.70, 0.15, 0.10, 0.03, 0.02)
  )

  # Ethnicity
  dm$ETHNIC <- sample(
    c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", NA_character_),
    n_patients,
    replace = TRUE,
    prob = c(0.15, 0.80, 0.05)
  )

  # Treatment arms
  arm_codes <- sapply(arms, function(x) x$code)
  arm_descs <- sapply(arms, function(x) x$description)

  dm$ARMCD <- sample(arm_codes, n_patients, replace = TRUE)
  dm$ARM <- arm_descs[match(dm$ARMCD, arm_codes)]

  # Actual arm (usually same as planned, but 2% crossover)
  dm$ACTARMCD <- dm$ARMCD
  dm$ACTARM <- dm$ARM

  crossover_idx <- sample(1:n_patients, size = ceiling(n_patients * 0.02))
  for (idx in crossover_idx) {
    new_arm <- sample(arm_codes[arm_codes != dm$ARMCD[idx]], 1)
    dm$ACTARMCD[idx] <- new_arm
    dm$ACTARM[idx] <- arm_descs[match(new_arm, arm_codes)]
  }

  # Country (ISO 3166 Alpha-3)
  dm$COUNTRY <- sample(
    c("USA", "CAN", "GBR", "DEU", "FRA", "JPN", "AUS"),
    n_patients,
    replace = TRUE,
    prob = c(0.40, 0.15, 0.10, 0.10, 0.10, 0.10, 0.05)
  )

  # Collection date and study day (demographics collected at screening)
  dm$DMDTC <- dm$RFICDTC  # Demographics collected at informed consent
  dm$DMDY <- as.integer(lubridate::ymd(dm$DMDTC) - lubridate::ymd(dm$RFSTDTC) + 1)

  # Reorder columns according to specification
  dm <- dm %>%
    select(
      STUDYID, DOMAIN, USUBJID, SUBJID,
      RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC, RFICDTC, RFPENDTC,
      DTHDTC, DTHFL, SITEID, INVID, INVNAM, BRTHDTC,
      AGE, AGEU, SEX, RACE, ETHNIC,
      ARMCD, ARM, ACTARMCD, ACTARM, COUNTRY,
      DMDTC, DMDY
    )

  # Apply metadata
  metadata <- list(
    STUDYID = "Study Identifier",
    DOMAIN = "Domain Abbreviation",
    USUBJID = "Unique Subject Identifier",
    SUBJID = "Subject Identifier for the Study",
    RFSTDTC = "Subject Reference Start Date/Time",
    RFENDTC = "Subject Reference End Date/Time",
    RFXSTDTC = "Date/Time of First Study Treatment",
    RFXENDTC = "Date/Time of Last Study Treatment",
    RFICDTC = "Date/Time of Informed Consent",
    RFPENDTC = "Date/Time of End of Participation",
    DTHDTC = "Date/Time of Death",
    DTHFL = "Subject Death Flag",
    SITEID = "Study Site Identifier",
    INVID = "Investigator Identifier",
    INVNAM = "Investigator Name",
    BRTHDTC = "Date/Time of Birth",
    AGE = "Age",
    AGEU = "Age Units",
    SEX = "Sex",
    RACE = "Race",
    ETHNIC = "Ethnicity",
    ARMCD = "Planned Arm Code",
    ARM = "Description of Planned Arm",
    ACTARMCD = "Actual Arm Code",
    ACTARM = "Description of Actual Arm",
    COUNTRY = "Country",
    DMDTC = "Date/Time of Collection",
    DMDY = "Study Day of Collection"
  )

  apply_metadata(dm, metadata)
}

#' Get Arms for Study
#'
#' @description
#' Helper function to get standard arm definitions
#'
#' @param type Type of study arms to generate
#'
#' @return List of arm definitions
#' @export
get_study_arms <- function(type = c("standard", "dose_escalation", "factorial")) {
  type <- match.arg(type)

  switch(type,
         standard = list(
           list(code = "PBO", description = "Placebo"),
           list(code = "TRT", description = "Treatment")
         ),
         dose_escalation = list(
           list(code = "PBO", description = "Placebo"),
           list(code = "TRT10", description = "Treatment 10mg"),
           list(code = "TRT25", description = "Treatment 25mg"),
           list(code = "TRT50", description = "Treatment 50mg"),
           list(code = "TRT100", description = "Treatment 100mg")
         ),
         factorial = list(
           list(code = "A", description = "Drug A"),
           list(code = "B", description = "Drug B"),
           list(code = "AB", description = "Drug A + Drug B"),
           list(code = "PBO", description = "Placebo")
         )
  )
}
