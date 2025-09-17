# R/se.R
#' Create Subject Elements (SE) Dataset from DM and TE
#'
#' @description
#' Dynamically generates an SDTM-compliant SE dataset using subject-level data from DM and element structure from TE.
#' Each subject's SE records are derived based on the TE sequence and planned durations.
#'
#' @param dm A data.frame representing the DM dataset, ideally from `rdm()`
#' @param te A data.frame representing the TE dataset, ideally from `rte()`
#'
#' @return A data.frame with SDTM SE structure
#' @export
#'
#' @examples
#' dm <- rdm(n_patients = 100, seed = 2025)
#' rte(etcd = c("SCR", "TRT", "FUP"),
#'     element = c("Screening", "Treatment", "Follow-up"),
#'     testrl = c("First visit", "First dose", "End of treatment"),
#'     teenrl = c("First dose", "End of treatment", "End of study"),
#'     tedur = c("14 days", "10 weeks", "4 weeks"))
#' se <- rse(dm, te)
#' head(se)

rse <- function(dm, te) {
  # Validate inputs
  assert_data_frame(dm)
  assert_data_frame(te)

  # Ensure RFXSTDTC is in Date format
  dm_se <- dm %>%
    filter(!is.na(RFXSTDTC)) %>%
    mutate(RFXSTDTC = ymd(RFXSTDTC))

  # Convert TEDUR to numeric duration in days
  te_se <- te %>%
    mutate(
      DURATION_DAYS = case_when(
        grepl("day", TEDUR, ignore.case = TRUE) ~ as.numeric(str_extract(TEDUR, "\\d+")),
        grepl("week", TEDUR, ignore.case = TRUE) ~ as.numeric(str_extract(TEDUR, "\\d+")) * 7,
        grepl("month", TEDUR, ignore.case = TRUE) ~ as.numeric(str_extract(TEDUR, "\\d+")) * 30,
        TRUE ~ NA_real_
      )
    )

  # Generate SE records dynamically
  se <- dm_se %>%
    rowwise() %>%
    do({
      subj <- .
      start_date <- subj$RFXSTDTC
      se_records <- list()

      for (i in seq_len(nrow(te_se))) {
        element <- te_se[i, ]
        end_date <- start_date + days(element$DURATION_DAYS)

        se_records[[i]] <- data.frame(
          STUDYID  = subj$STUDYID,
          DOMAIN   = "SE",
          USUBJID  = subj$USUBJID,
          SESEQ    = i,
          ETCD     = element$ETCD,
          ELEMENT  = element$ELEMENT,
          SESTDTC  = format(start_date, "%Y-%m-%d"),
          SEENDTC  = format(end_date, "%Y-%m-%d"),
          TAETORD  = i,
          EPOCH    = element$ELEMENT,
          SEUPDES  = "",
          SESTDY   = as.integer(difftime(start_date, subj$RFXSTDTC, units = "days")),
          SEENDY   = as.integer(difftime(end_date, subj$RFXSTDTC, units = "days"))
        )

        start_date <- end_date + days(1)  # next element starts the day after
      }

      bind_rows(se_records)
    }) %>%
    ungroup()

  # Apply metadata
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

  # Assign sequence numbers
  se <- seqnum(se, sort = c("USUBJID", "SESTDTC"))

  return(se)
}

# Call the function to create the dataset
se <- rse(dm, te)



