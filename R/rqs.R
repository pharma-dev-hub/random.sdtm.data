#' QS Dataset
#'
#' @description
#' Generates a synthetic SDTM-compliant QS dataset using subject-level data from DM.
#' Includes realistic questionnaire terms, categories, subcategories, and timing aligned with study visits.
#'
#' @param dm A data.frame representing the DM dataset
#' @param seed Optional random seed for reproducibility
#' @param qs_terms A tibble or data.frame of questionnaire definitions with columns:
#'   QSTESTCD (short code), QSTEST (full name), QSCAT (category), QSSCAT (subcategory).
#'   Defaults include common PRO measures (Pain, Mood, Sleep).
#'
#' @return A data.frame with SDTM QS structure
#' @export
#'
#' @examples
#' dm <- rdm(n_patients = 100)
#' qs <- rqs(dm)

rqs <- function(dm, seed = 2025,
                qs_terms = tibble::tibble(
                  QSTESTCD = c("PAIN01", "PAIN02", "MOOD01", "MOOD02", "SLEEP01"),
                  QSTEST   = c("Pain Severity", "Pain Interference", "Mood Rating", "Anxiety Level", "Sleep Quality"),
                  QSCAT    = c("Pain", "Pain", "Mood", "Mood", "Sleep"),
                  QSSCAT   = c("Severity", "Interference", "General", "General", "General")
                )) {

  if (!is.null(seed)) set.seed(seed)

  dm_qs <- dm %>% mutate(RFSTDTC = as.Date(RFSTDTC))

  # Select 50% of subjects and assign random QS counts
  qs_subjects <- dm_qs %>%
    sample_frac(0.5) %>%
    rowwise() %>%
    mutate(n_qs = sample(3:5, 1)) %>%
    ungroup()

  # Expand rows based on n_qs
  qs <- qs_subjects %>%
    slice(rep(1:n(), qs_subjects$n_qs)) %>%
    group_by(USUBJID) %>%
    mutate(
      QSSEQ      = row_number(),
      TERM_INDEX = sample(1:nrow(qs_terms), n(), replace = TRUE),
      QSTESTCD   = qs_terms$QSTESTCD[TERM_INDEX],
      QSTEST     = qs_terms$QSTEST[TERM_INDEX],
      QSCAT      = qs_terms$QSCAT[TERM_INDEX],
      QSSCAT     = qs_terms$QSSCAT[TERM_INDEX],
      QSORRES    = case_when(
        QSCAT == "Pain"  ~ sample(c("None", "Mild", "Moderate", "Severe"), 1),
        QSCAT == "Mood"  ~ sample(c("Happy", "Neutral", "Sad", "Anxious"), 1),
        QSCAT == "Sleep" ~ sample(c("Good", "Fair", "Poor"), 1)
      ),
      QSSTRESC   = QSORRES,
      QSSTRESN   = case_when(
        QSORRES %in% c("None", "Happy", "Good") ~ 0,
        QSORRES %in% c("Mild", "Neutral", "Fair") ~ 1,
        QSORRES %in% c("Moderate", "Sad", "Poor") ~ 2,
        QSORRES %in% c("Severe", "Anxious") ~ 3
      ),
      QSSTAT     = NA,
      QSREASND   = NA,
      QSBLFL     = sample(c("Y", NA), 1, prob = c(0.2, 0.8)),
      VISITNUM   = sample(1:5, 1),
      VISIT      = paste("Visit", VISITNUM),
      VISITDY    = VISITNUM * 7,
      QSDTC      = RFSTDTC + VISITDY,
      QSDY       = as.integer(QSDTC - RFSTDTC + 1),
      DOMAIN     = "QS",
      STUDYID    = STUDYID[1],
      QSSPID     = NA
    ) %>%
    arrange(USUBJID, QSDTC) %>%
    ungroup()

  # Assign EPOCH and QSSEQ
  qs <- epoch(qs, dtc = "QSDTC")
  qs <- seqnum(qs, sort = c("USUBJID", "QSDTC"))

  # Apply SDTM length constraints
  qs <- qs %>%
    mutate(
      STUDYID   = substr(STUDYID, 1, 6),
      DOMAIN    = substr(DOMAIN, 1, 2),
      USUBJID   = substr(USUBJID, 1, 16),
      QSSPID    = substr(QSSPID, 1, 2),
      QSTESTCD  = substr(QSTESTCD, 1, 8),
      QSTEST    = substr(QSTEST, 1, 40),
      QSCAT     = substr(QSCAT, 1, 40),
      QSSCAT    = substr(QSSCAT, 1, 21),
      QSORRES   = substr(QSORRES, 1, 56),
      QSSTRESC  = substr(QSSTRESC, 1, 31),
      EPOCH     = substr(EPOCH, 1, 9)
    ) %>%
    select(
      STUDYID, DOMAIN, USUBJID, QSSEQ, QSSPID, QSTESTCD, QSTEST, QSCAT, QSSCAT,
      QSORRES, QSSTRESC, QSSTRESN, QSSTAT, QSREASND, QSBLFL,
      VISITNUM, VISIT, VISITDY, EPOCH, QSDTC, QSDY
    )

  # Apply metadata
  qs <- apply_metadata(qs, list(
    STUDYID   = "Study Identifier",
    DOMAIN    = "Domain Abbreviation",
    USUBJID   = "Unique Subject Identifier",
    QSSEQ     = "Sequence Number",
    QSSPID    = "Sponsor-Defined Identifier",
    QSTESTCD  = "Question Short Name",
    QSTEST    = "Question Name",
    QSCAT     = "Category of Question",
    QSSCAT    = "Subcategory for Question",
    QSORRES   = "Finding in Original Units",
    QSSTRESC  = "Character Result/Finding in Std Format",
    QSSTRESN  = "Numeric Finding in Standard Units",
    QSSTAT    = "Completion Status",
    QSREASND  = "Reason Not Performed",
    QSBLFL    = "Baseline Flag",
    VISITNUM  = "Visit Number",
    VISIT     = "Visit Name",
    VISITDY   = "Planned Study Day of Visit",
    EPOCH     = "Epoch",
    QSDTC     = "Date/Time of Finding",
    QSDY      = "Study Day of Finding"
  ))

  return(qs)
}

