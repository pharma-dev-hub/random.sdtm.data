#' Create Adverse Events (AE) Dataset from DM and SE
#'
#' @description
#' Generates a synthetic SDTM-compliant AE dataset using subject-level data from DM and SE.
#' Includes realistic AE terms, coding hierarchy, and temporal alignment with study dates.
#'
#' @param dm A data.frame representing the DM dataset
#' @param se A data.frame representing the SE dataset
#'
#' @return A data.frame with SDTM AE structure
#' @export
#'
#' @examples
#' dm <- rdm(n_patients = 100)
#' se <- rse(dm, te)
#' ae <- rae(dm, se)


rae <- function(dm, se, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  # AE terms and coding hierarchy
  ae_terms <- tibble(
    AETERM   = c("Headache", "Nausea", "Rash", "Fatigue", "Injection site pain"),
    AEDECOD  = c("HEADACHE", "NAUSEA", "RASH", "FATIGUE", "INJECTION SITE PAIN"),
    AESOC    = c("Nervous system disorders", "Gastrointestinal disorders", "Skin disorders", "General disorders", "General disorders"),
    AEHLT    = c("Headaches", "GI symptoms", "Skin reactions", "Fatigue symptoms", "Injection site reactions"),
    AEHLGT   = c("Neurological symptoms", "GI disorders", "Dermatologic conditions", "General symptoms", "Injection reactions"),
    AELLT    = c("Tension headache", "Mild nausea", "Contact rash", "Exhaustion", "Localized pain"),
    AEPTCD   = 1001:1005,
    AELLTCD  = 2001:2005,
    AEHLTCD  = 3001:3005,
    AEHLGTCD = 4001:4005,
    AESOCCD  = 5001:5005,
    AEBDSYCD = 6001:6005
  )

  dm_ae <- dm %>% mutate(RFSTDTC = as.Date(RFSTDTC))

  # Select 20% of subjects to have AEs
  ae_subjects <- dm_ae %>% sample_frac(0.2)

  # Generate AE records
  ae <- ae_subjects %>%
    slice(rep(1:n(), each = sample(1:3, 1))) %>%
    group_by(USUBJID) %>%
    mutate(
      TERM_INDEX = sample(1:nrow(ae_terms), n(), replace = TRUE),
      AETERM     = ae_terms$AETERM[TERM_INDEX],
      AEMODIFY   = AETERM,
      AEDECOD    = ae_terms$AEDECOD[TERM_INDEX],
      AESOC      = ae_terms$AESOC[TERM_INDEX],
      AEHLT      = ae_terms$AEHLT[TERM_INDEX],
      AEHLGT     = ae_terms$AEHLGT[TERM_INDEX],
      AELLT      = ae_terms$AELLT[TERM_INDEX],
      AEPTCD     = ae_terms$AEPTCD[TERM_INDEX],
      AELLTCD    = ae_terms$AELLTCD[TERM_INDEX],
      AEHLTCD    = ae_terms$AEHLTCD[TERM_INDEX],
      AEHLGTCD   = ae_terms$AEHLGTCD[TERM_INDEX],
      AESOCCD    = ae_terms$AESOCCD[TERM_INDEX],
      AEBDSYCD   = ae_terms$AEBDSYCD[TERM_INDEX],
      AESEV      = sample(c("MILD", "MOD", "SEV"), n(), replace = TRUE),
      AESER      = sample(c("Y", "N"), n(), replace = TRUE, prob = c(0.1, 0.9)),
      AEOUT      = sample(c("RECOVERED", "ONGOING", "DEATH"), n(), replace = TRUE),
      AEACN      = sample(c("NONE", "DOSE REDUCED", "DRUG WITHDRAWN"), n(), replace = TRUE),
      AEACNOTH   = NA,
      AEREL      = sample(c("RELATED", "NOT RELATED"), n(), replace = TRUE),
      AEPATT     = sample(c("INTERMITTENT", "CONTINUOUS", "UNKNOWN"), n(), replace = TRUE),
      AESTDTC    = RFSTDTC + sample(0:70, n(), replace = TRUE),
      AEENDTC    = AESTDTC + sample(1:5, n(), replace = TRUE),
      AESTDY     = as.integer(AESTDTC - RFSTDTC + 1),
      AEENDY     = as.integer(AEENDTC - RFSTDTC + 1),
      AEENRF     = "AFTER",
      AEENRTPT   = "TREATMENT",
      AEENTPT    = "END OF TREATMENT",
      DOMAIN     = "AE",
      STUDYID    = STUDYID[1],
      AESPID     = NA
    ) %>%
    ungroup()

  # Assign EPOCH and AESEQ
  ae <- epoch(ae, dtc = "AESTDTC")
  ae <- seqnum(ae, sort = c("USUBJID", "AESTDTC"))

  # Apply SDTM length constraints
  ae <- ae %>%
    mutate(
      STUDYID    = substr(STUDYID, 1, 6),
      DOMAIN     = substr(DOMAIN, 1, 2),
      USUBJID    = substr(USUBJID, 1, 16),
      AESPID     = substr(AESPID, 1, 2),
      AETERM     = substr(AETERM, 1, 68),
      AEMODIFY   = substr(AEMODIFY, 1, 68),
      AELLT      = substr(AELLT, 1, 39),
      AEDECOD    = substr(AEDECOD, 1, 39),
      AEHLT      = substr(AEHLT, 1, 70),
      AEHLGT     = substr(AEHLGT, 1, 60),
      AESOC      = substr(AESOC, 1, 67),
      AEBODSYS   = AESOC,
      AESEV      = substr(AESEV, 1, 8),
      AESER      = substr(AESER, 1, 1),
      AEACN      = substr(AEACN, 1, 16),
      AEACNOTH   = substr(AEACNOTH, 1, 4),
      AEREL      = substr(AEREL, 1, 11),
      AEPATT     = substr(AEPATT, 1, 12),
      AEOUT      = substr(AEOUT, 1, 32),
      EPOCH      = substr(EPOCH, 1, 9)
    ) %>%
    select(
      STUDYID, DOMAIN, USUBJID, AESEQ, AESPID, AETERM, AEMODIFY, AELLT, AELLTCD,
      AEDECOD, AEPTCD, AEHLT, AEHLTCD, AEHLGT, AEHLGTCD, AEBODSYS, AEBDSYCD,
      AESOC, AESOCCD, AESEV, AESER, AEACN, AEACNOTH, AEREL, AEPATT, AEOUT,
      EPOCH, AESTDTC, AEENDTC, AESTDY, AEENDY, AEENRF, AEENRTPT, AEENTPT
    )

  # Apply metadata
  ae <- apply_metadata(ae, list(
    STUDYID    = "Study Identifier",
    DOMAIN     = "Domain Abbreviation",
    USUBJID    = "Unique Subject Identifier",
    AESPID     = "Sponsor-Defined Identifier",
    AETERM     = "Reported Term for the Adverse Event",
    AEMODIFY   = "Modified Reported Term",
    AELLT      = "Lowest Level Term",
    AELLTCD    = "Lowest Level Term Code",
    AEDECOD    = "Dictionary-Derived Term",
    AEPTCD     = "Preferred Term Code",
    AEHLT      = "High Level Term",
    AEHLTCD    = "High Level Term Code",
    AEHLGT     = "High Level Group Term",
    AEHLGTCD   = "High Level Group Term Code",
    AEBODSYS   = "Body System or Organ Class",
    AEBDSYCD   = "Body System or Organ Class Code",
    AESOC      = "Primary System Organ Class",
    AESOCCD    = "Primary System Organ Class Code",
    AESEV      = "Severity/Intensity",
    AESER      = "Serious Event",
    AEACN      = "Action taken with Study Treatment",
    AEACNOTH   = "Other Action Taken",
    AEREL      = "Causality",
    AEPATT     = "Pattern of Adverse Event",
    AEOUT      = "Outcome of Adverse Event",
    EPOCH      = "Epoch",
    AESTDTC    = "Start Date/Time of Adverse Event",
    AEENDTC    = "End Date/Time of Adverse Event",
    AESTDY     = "Study Day of Start of Adverse Event",
    AEENDY     = "Study Day of End of Adverse Event",
    AEENRF     = "End Relative to Reference Period",
    AEENRTPT   = "End Relative to Reference Time Point",
    AEENTPT    = "End Reference Time Point"
  ))

  return(ae)
}

ae <- rae(dm, se)
