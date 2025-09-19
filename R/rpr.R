# R/rpr.R
#' Create Random Procedures (PR) Dataset
#'
#' @description
#' Creates a random SDTM PR (Procedures) dataset following CDISC SDTM standards.
#' Interventions. One record per recorded procedure per occurrence per subject, Tabulation.
#' The following Permissible variables have NOT been mapped within the rpr function: PRGRPID, PRSPID, PRLNKID, PRLNKGRP, PRINDC, PRDOSRGM, PRDIR, PRPORTOT, VISITDY, TAETORD, PRDUR, PRTPT, PRTPTNUM, PRELTM, PRTPTREF, PRRFTDTC, PRSTRTPT, PRSTTPT, PRENRTPT, PRENTPT.
#' Dependency datasets: dm, se, sv
#'
#' @param domain By default, value has been set as "PR", user can modify it if needed but not recommended
#' @param trt Values to be mapped under PRTRT variable
#' @param visn VISITNUM values for each TRT value (optional). If you need more than one VISITNUM per TRT, then it should be separated by comma within quotes, ex: c("1, 2, 3") - this will be considered as three VISITNUM values for the first TRT value. If NA values or no values provided for VISN parameter, minimum VISITNUM from tv dataset would be auto-populated.
#' @param cat Values to be mapped under PRCAT variable (optional)
#' @param scat Values to be mapped under PRSCAT variable (optional)
#' @param presp Values to be mapped under PRPRESP variable (optional)
#' @param dose Numeric vector - Values to be mapped under PRDOSE variable (optional)
#' @param dostxt Values to be mapped under PRDOSTXT variable (optional)
#' @param dosu Values to be mapped under PRDOSU variable (optional)
#' @param dosfrm Values to be mapped under PRDOSFRM variable (optional)
#' @param dosfrq Values to be mapped under PRDOSFRQ variable (optional)
#' @param route Values to be mapped under PRROUTE variable (optional)
#' @param loc Values to be mapped under PRLOC variable (optional)
#' @param lat Values to be mapped under PRLAT variable (optional)
#' @param sort_seq Sorting sequence to be used for PRSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "PRTRT", "PRDECOD", "PRSTDTC", "PRENDTC"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be listed in UPPERCASE
#'
#' @return
#' @export
#'
#' @examples
#' rpr() #Creates PR dataset with predefined default values
#'
#' rpr(trt = c("Biopsy", "CT scan", "MRI", "X-Ray"),
#'     visn = c("1", "1, 2, 3, 99", NA, NA),
#'     cat = c(NA, "Imaging", "Imaging", "Imaging"),
#'     scat = c(NA, "Computed Tomography", "Magnetic Resonance Imaging", "Radiography"),
#'     presp = c("Y", "Y", NA, NA),
#'     loc = c(NA, "Chest", "Brain", "Knee"),
#'     lat = c(NA, NA, NA, "LEFT"),
#'     sort_seq = c("STUDYID", "USUBJID", "PRTRT", "PRDECOD", "PRSTDTC", "PRENDTC"))
#'

rpr <- function(domain = "PR",
                trt = c("Biopsy", "CT scan", "MRI", "X-Ray"),
                visn = c("1", "1, 2, 3, 99", NA, NA),
                cat = c(NA, "Imaging", "Imaging", "Imaging"),
                scat = c(NA, "Computed Tomography", "Magnetic Resonance Imaging", "Radiography"),
                presp = c("Y", "Y", NA, NA),
                dose = c(),
                dostxt = c(),
                dosu = c(),
                dosfrm = c(),
                dosfrq = c(),
                route = c(),
                loc = c(NA, "Chest", "Brain", "Knee"),
                lat = c(NA, NA, NA, "LEFT"),
                sort_seq = c("STUDYID", "USUBJID", "PRTRT", "PRDECOD", "PRSTDTC", "PRENDTC"),
                drop_vars = c()
                ) {

  # Metadata for the PR dataset
  pr_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "PRSEQ" = "Sequence Number",
                      "PRGRPID" = "Group ID",
                      "PRSPID" = "Sponsor-Defined Identifier",
                      "PRLNKID" = "Link ID",
                      "PRLNKGRP" = "Link Group ID",
                      "PRTRT" = "Reported Name of Procedure",
                      "PRDECOD" = "Standardized Procedure Name",
                      "PRCAT" = "Category",
                      "PRSCAT" = "Subcategory",
                      "PRPRESP" = "Pre-specified",
                      "PROCCUR" = "Occurrence",
                      "PRINDC" = "Indication",
                      "PRDOSE" = "Dose",
                      "PRDOSTXT" = "Dose Description",
                      "PRDOSU" = "Dose Units",
                      "PRDOSFRM" = "Dose Form",
                      "PRDOSFRQ" = "Dosing Frequency per Interval",
                      "PRDOSRGM" = "Intended Dose Regimen",
                      "PRROUTE" = "Route of Administration",
                      "PRLOC" = "Location of Procedure",
                      "PRLAT" = "Laterality",
                      "PRDIR" = "Directionality",
                      "PRPORTOT" = "Portion or Totality",
                      "VISITNUM" = "Visit Number",
                      "VISIT" = "Visit Name",
                      "VISITDY" = "Planned Study Day of Visit",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "PRSTDTC" = "Start Date/Time of Procedure",
                      "PRENDTC" = "End Date/Time of Procedure",
                      "PRSTDY" = "Study Day of Start of Procedure",
                      "PRENDY" = "Study Day of End of Procedure",
                      "PRDUR" = "Duration of Procedure",
                      "PRTPT" = "Planned Time Point Name",
                      "PRTPTNUM" = "Planned Time Point Number",
                      "PRELTM" = "Planned Elapsed Time from Time Point Ref",
                      "PRTPTREF" = "Time Point Reference",
                      "PRRFTDTC" = "Date/Time of Reference Time Point",
                      "PRSTRTPT" = "Start Relative to Reference Time Point",
                      "PRSTTPT" = "Start Reference Time Point",
                      "PRENRTPT" = "End Relative to Reference Time Point",
                      "PRENTPT" = "End Reference Time Point"
                      )

  # Checking the availability of the dependent datasets
  if (!exists("dm") | !exists("se") | !exists("sv")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, se, sv.")
  }

  # Checking if the required inputs have been provided
  if (is.null(trt)) {
    stop("Error: Empty inputs given for the following parameter: trt. Make sure to add values OR remove the parameter to use default values.")
  }

  # Combininig the TRT and Visitnum values
  # If Both the visn and trt are not provided then using the default values, if visn alone provided then checking if length matches default trt length, if visn is not provided mapping minimum VISITNUM from tv
  if (!missing(visn) & (length(trt) == length(visn)) | (missing(trt) & missing(visn))) {

    trt_visn_df1 <- data.frame(trt, visn)

  } else if (!missing(visn) & (length(trt) != length(visn))) {

    stop("Error: No. of TRT and VISN provided not matched")

  } else if (missing(visn)) {

    trt_visn_df1 <- as.data.frame(trt) %>% mutate(visn = min(tv$VISITNUM, na.rm = TRUE))
  }

  # Checking when TRT is provided but optional parameters (which has default values) are not provided, in that case default values for the optional parameteres will be converted to NAs.
    if (!missing(trt) & missing(cat)) {
      cat <- rep(NA, length(trt))
    } else if (!missing(cat)) {
      cat <- opt_var_chk(opt_var = "cat", comp_var = "trt")  # Comparing CAT and TRT inputs
    }

    if (!missing(trt) & missing(scat)) {
      scat <- rep(NA, length(trt))
    } else if (!missing(scat)) {
      scat <- opt_var_chk(opt_var = "scat", comp_var = "trt") # Comparing SCAT and TRT inputs
    }

    if (!missing(trt) & missing(presp)) {
      presp <- rep(NA, length(trt))
    } else if (!missing(presp)) {
      presp <- opt_var_chk(opt_var = "presp", comp_var = "trt")  # Comparing PRESP and TRT inputs
    }

    if (!missing(trt) & missing(loc)) {
      loc <- rep(NA, length(trt))
    } else if (!missing(loc)) {
      loc <- opt_var_chk(opt_var = "loc", comp_var = "trt")  # Comparing LOC and TRT inputs
    }

    if (!missing(trt) & missing(lat)) {
      lat <- rep(NA, length(trt))
    } else if (!missing(lat)) {
      lat <- opt_var_chk(opt_var = "lat", comp_var = "trt")  # Comparing LAT and TRT inputs
    }

  # Checking if PRESP is provided with allowed values
  if (!missing(presp) & !all(presp %in% c("N", "NA", "U", "Y", NA))) {
    stop("Error: Invalid input for presp. The allowed values are: N, NA, U, Y")
  }

  # Checking if input for DOSE is provided with numeric values
  if (!is.null(dose) & !is.numeric(dose)) {
    stop("Error: DOSE should be provided with numeric values")
  }

  # Checking if all the optional parameters (without default values) has necessary input values or else creating dummy values
  dose <- opt_var_chk(opt_var = "dose", comp_var = "trt")  # Comparing DOSE and TRT inputs
  dostxt <- opt_var_chk(opt_var = "dostxt", comp_var = "trt")  # Comparing DOSTXT and TRT inputs
  dosu <- opt_var_chk(opt_var = "dosu", comp_var = "trt")  # Comparing DOSU and TRT inputs
  dosfrm <- opt_var_chk(opt_var = "dosfrm", comp_var = "trt")  # Comparing DOSFRM and TRT inputs
  dosfrq <- opt_var_chk(opt_var = "dosfrq", comp_var = "trt")  # Comparing DOSFRQ and TRT inputs
  route <- opt_var_chk(opt_var = "route", comp_var = "trt")  # Comparing ROUTE and TRT inputs

  # Data frame for the optional variable
  opt_vars <- data.frame(cat, scat, presp, dose, dostxt, dosu, dosfrm, dosfrq, route, loc, lat)

  df1 <- cbind(trt_visn_df1, opt_vars)

  # Creating rows for each Visitnum values and If no Visitnum value is given assigning it with minimum VISITNUM value from tv
  df2 <- df1 %>%
         mutate(row_id = row_number()) %>%
         separate_rows(visn, sep = ",\\s*", convert = TRUE) %>%
         group_by(row_id) %>%
         mutate(vis_index = row_number()) %>%
         ungroup() %>%
         mutate(VISITNUM = ifelse(!is.na(visn) & as.character(visn) != "", as.numeric(visn), min(tv$VISITNUM, na.rm = TRUE)))

  # Crossing dm with trt dataframe to get trt for each subjects
  df3 <- crossing(dm %>% select(USUBJID), df2)

  # Input SV dataset to create data for each visit occurred
  sv_in <- sv %>%
    select(USUBJID, VISIT, VISITNUM, SVOCCUR, SVSTDTC, SVENDTC)

  # Joining with SV to get the VISIT Occurance details and to keep only records which has data from SV
  df4 <- df3 %>% left_join(sv_in, by = c("USUBJID" = "USUBJID", "VISITNUM" = "VISITNUM")) %>%
         filter(!is.na(VISIT))

  # Mapping general variables
  df5 <- df4 %>%
         mutate(STUDYID = studyid,
                DOMAIN = domain,
                PRTRT = trt,
                PRDECOD = toupper(trt),
                PRCAT = cat,
                PRSCAT = scat,
                PRPRESP = presp,
                PROCCUR = ifelse(!is.na(PRPRESP) & PRPRESP != "", SVOCCUR, NA),
                PRDOSE = as.numeric(dose),
                PRDOSTXT = dostxt,
                PRDOSU = dosu,
                PRDOSFRM = dosfrm,
                PRDOSFRQ = dosfrq,
                PRROUTE = route,
                PRLOC = loc,
                PRLAT = lat,
                PRSTDTC = SVSTDTC,
                PRENDTC = SVENDTC
                )

  # Mapping EPOCH variable
  df6 <- epoch(df = df5, dtc = "PRSTDTC")

  # Mapping --DY variable
  df7 <- stdy(df = df6, dtc = "PRSTDTC")
  df8 <- stdy(df = df7, dtc = "PRENDTC")

  # Mapping SEQ variable
  df9 <- seqnum(df = df8, sort = sort_seq)

  # Keeping only the Necessary variables
  df10 <- df9 %>%
          select(STUDYID, DOMAIN, USUBJID, PRSEQ, PRTRT, PRDECOD, PRCAT, PRSCAT, PRPRESP, PROCCUR, PRDOSE, PRDOSTXT, PRDOSU, PRDOSFRM, PRDOSFRQ, PRROUTE,
                 PRLOC, PRLAT, VISITNUM, VISIT, EPOCH, PRSTDTC, PRENDTC, PRSTDY, PRENDY)

  # Adding labels to the variables
  df11 <- apply_metadata(df10, pr_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    df11 <- df11 %>% select(-all_of(drop_vars))
  }

  # Final PR dataset
  assign("pr", df11, envir = .GlobalEnv)

}
