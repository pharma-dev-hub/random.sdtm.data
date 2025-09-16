#' Create Exposure (EX) Dataset from DM
#'
#' @description
#' Generates an SDTM-compliant EX dataset using subject-level data from DM and a predefined dosing schedule.
#' Includes treatment details based on ARMCD and applies metadata, epoch, and sequence numbering.
#'
#' @param dm A data.frame representing the DM dataset
#'
#' @return A data.frame with SDTM EX structure
#' @export
#'
#' @examples
#' ex <- rex(dm)

rex <- function(dm) {
  assert_data_frame(dm)

  dm_ex <- dm %>%
    mutate(
      RFXSTDTC = as.Date(RFXSTDTC),
      RFXENDTC = as.Date(RFXENDTC)
    )

  # Define dosing schedule: every 2 weeks for 10 weeks
  dose_days <- seq(0, 56, by = 14)

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
  ex <- dm_ex %>%
    rowwise() %>%
    do({
      subject <- .
      arm <- subject$ACTARMCD
      trt <- filter(treatment_details, ARMCD == arm)

      tibble(
        STUDYID   = subject$STUDYID,
        DOMAIN    = "EX",
        USUBJID   = subject$USUBJID,
        EXSPID    = "",
        EXTRT     = trt$EXTRT,
        EXCAT     = trt$EXCAT,
        EXDOSE    = trt$EXDOSE,
        EXDOSU    = trt$EXDOSU,
        EXDOSFRM  = trt$EXDOSFRM,
        EXROUTE   = trt$EXROUTE,
        EXLOC     = trt$EXLOC,
        EXLAT     = trt$EXLAT,
        EXTRTV    = trt$EXTRTV,
        VISITNUM  = seq_along(dose_days),
        VISIT     = paste("Week", dose_days / 7),
        VISITDY   = dose_days,
        EXSTDTC   = subject$RFXSTDTC + days(dose_days),
        EXENDTC   = subject$RFXSTDTC + days(dose_days),
        EXSTDY    = dose_days + 1,
        EXENDY    = dose_days + 1
      )
    }) %>%
    ungroup()

  # Apply epoch and sequence number
  ex <- ex %>%
    epoch(dtc = "EXSTDTC") %>%
    seqnum(sort = c("USUBJID", "EXSTDTC"))

  # Apply metadata
  ex <- apply_metadata(ex, list(
    STUDYID   = "Study Identifier",
    DOMAIN    = "Domain Abbreviation",
    USUBJID   = "Unique Subject Identifier",
    EXSEQ     = "Sequence Number",
    EXSPID    = "Sponsor-Defined Identifier",
    EXTRT     = "Name of Actual Treatment",
    EXCAT     = "Category for Exposure",
    EXDOSE    = "Dose per Administration",
    EXDOSU    = "Dose Units",
    EXDOSFRM  = "Dose Form",
    EXROUTE   = "Route of Administration",
    EXLOC     = "Location of Administration",
    EXLAT     = "Laterality of Administration",
    EXTRTV    = "Treatment Vehicle",
    VISITNUM  = "Visit Number",
    VISIT     = "Visit Name",
    VISITDY   = "Planned Study Day of Visit",
    EPOCH     = "Epoch",
    EXSTDTC   = "Start Date/Time of Treatment",
    EXENDTC   = "End Date/Time of Treatment",
    EXSTDY    = "Study Day of Start of Treatment",
    EXENDY    = "Study Day of End of Treatment"
  ))

  # Order columns
  ex <- ex %>%
    select(
      STUDYID, DOMAIN, USUBJID, EXSEQ, EXSPID, EXTRT, EXCAT, EXDOSE, EXDOSU, EXDOSFRM,
      EXROUTE, EXLOC, EXLAT, EXTRTV, VISITNUM, VISIT, VISITDY, EPOCH,
      EXSTDTC, EXENDTC, EXSTDY, EXENDY
    )

  return(ex)
}

# Call the function to create the dataset
ex <- rex(dm)



