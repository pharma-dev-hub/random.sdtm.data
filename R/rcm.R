#' Create Random Concomitant Medications (CM) Dataset
#'
#' @description
#' Generates a synthetic SDTM CM dataset using subject-level data from DM.
#' Includes medication details, dates, and SDTM-compliant formatting.
#'
#' @param dm A data.frame from `rdm()` containing subject-level data
#' @param cm_terms A data.frame of medication terms with CMTRT, CMDECOD, CMCAT
#' @param max_meds_per_subject Maximum number of medications per subject
#' @param seed Random seed for reproducibility
#' @param na_prob Probability of NA values for optional fields
#'
#' @return A data.frame with SDTM CM structure
#' @export
#'
#' @examples
#' dm <- rdm(n_patients = 100, seed = 2025)
#' cm_terms <- tibble::tibble(
#'   CMTRT = c("Paracetamol", "Ibuprofen", "Metformin"),
#'   CMDECOD = c("PARACETAMOL", "IBUPROFEN", "METFORMIN"),
#'   CMCAT = c("Analgesic", "Analgesic", "Antidiabetic")
#' )
#' cm <- rcm(dm, cm_terms)

rcm <- function(dm,
                # cm_terms = tibble::tibble(
                #   CMTRT = c("Paracetamol", "Ibuprofen", "Metformin", "Atorvastatin", "Lisinopril"),
                #   CMDECOD = toupper(c("Paracetamol", "Ibuprofen", "Metformin", "Atorvastatin", "Lisinopril")),
                #   CMCAT = c("Analgesic", "Analgesic", "Antidiabetic", "Lipid-lowering", "Antihypertensive")
                # ),
                min_meds_per_subject = 1,
                max_meds_per_subject = 5,
                seed = NULL,
                na_prob = 0.05,
                death_dates = NULL) {

  if (!is.null(seed)) set.seed(seed)
  if (!inherits(dm$RFSTDTC, "Date")) dm$RFSTDTC <- as.Date(dm$RFSTDTC)

  n_total <- nrow(dm)
  n_subjects <- sample(1:n_total, 1)
  selected_subjects <- dm %>% dplyr::slice(sample(1:n_total, n_subjects))

  # For each selected subject, generate a random number of CM records
  cm_subjects <- selected_subjects %>%
    dplyr::rowwise() %>%
    dplyr::mutate(n_cm = sample(min_meds_per_subject:max_meds_per_subject, 1)) %>%
    dplyr::ungroup() %>%
    dplyr::slice(rep(1:dplyr::n(), .$n_cm))

  # If death_dates is provided, join it in
  if (!is.null(death_dates)) {
    cm_subjects <- cm_subjects %>%
      dplyr::left_join(death_dates, by = "USUBJID")
  } else {
    cm_subjects$death_date <- as.Date(NA)
  }

  cm <- cm_subjects %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(
      CMSPID = NA,
      TERM_INDEX = sample(1:nrow(cm_terms), dplyr::n(), replace = TRUE),
      CMTRT = cm_terms$CMTRT[TERM_INDEX],
      CMMODIFY = CMTRT,
      CMDECOD = cm_terms$CMDECOD[TERM_INDEX],
      CMCAT = cm_terms$CMCAT[TERM_INDEX],
      CMINDC = sample(c("Headache", "Pain", "Infection", "Allergy", "Diabetes"), dplyr::n(), replace = TRUE),
      CMDOSE = sample(c(500, 1000, 250), dplyr::n(), replace = TRUE),
      CMDOSU = sample(c("mg", "ml", "tablet"), dplyr::n(), replace = TRUE),
      CMDOSFRM = sample(c("Tablet", "Capsule", "Liquid"), dplyr::n(), replace = TRUE),
      CMDOSFRQ = sample(c("BID", "QD", "PRN"), dplyr::n(), replace = TRUE),
      CMROUTE = sample(c("Oral", "Topical", "SC"), dplyr::n(), replace = TRUE),
      # Generate CMSTDTC, but cap at death_date if present
      CMSTDTC_raw = RFSTDTC + sample(0:60, dplyr::n(), replace = TRUE),
      CMSTDTC = as.Date(ifelse(!is.na(death_date) & CMSTDTC_raw > death_date, as.character(death_date), as.character(CMSTDTC_raw))),
      CMENDTC_raw = CMSTDTC + sample(5:30, dplyr::n(), replace = TRUE),
      CMENDTC = as.Date(ifelse(!is.na(death_date) & CMENDTC_raw > death_date, as.character(death_date), as.character(CMENDTC_raw))),
      CMSTDY = as.integer(CMSTDTC - RFSTDTC + 1),
      CMENDY = as.integer(CMENDTC - RFSTDTC + 1),
      CMENRF = "AFTER",
      CMENRTPT = "TREATMENT",
      CMENTPT = "END OF TREATMENT",
      DOMAIN = "CM",
      STUDYID = STUDYID[1]
    ) %>%
    dplyr::ungroup()

  # Assign EPOCH and SEQ
  cm <- epoch(cm, dtc = "CMSTDTC")
  cm <- seqnum(cm, sort = c("USUBJID", "CMSTDTC"))

  # Apply SDTM length constraints and select columns as before...
  cm <- cm %>%
    dplyr::mutate(
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
    dplyr::select(
      STUDYID, DOMAIN, USUBJID, CMSEQ, CMSPID, CMTRT, CMMODIFY, CMDECOD, CMCAT,
      CMINDC, CMDOSE, CMDOSU, CMDOSFRM, CMDOSFRQ, CMROUTE, EPOCH,
      CMSTDTC, CMENDTC, CMSTDY, CMENDY, CMENRF, CMENRTPT, CMENTPT
    )

  # Apply metadata
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

  return(cm)
}

cm <- rcm(dm)
