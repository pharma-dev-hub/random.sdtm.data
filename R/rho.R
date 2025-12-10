# R/rho.R
#' Create Random Healthcare Encounters (HO) Dataset
#'
#' @description
#' Creates a random SDTM HO (Healthcare Encounters) dataset following CDISC SDTM standards.
#' Events. One record per healthcare encounter per subject, Tabulation.
#' The following Permissible variables have NOT been mapped within the rho function: HOGRPID, HOREFID, HOSPID, HODECOD, HOSTAT, HOREASND, TAETORD, HODTC, HODY, HODUR, HOSTRTPT, HOSTTPT, HOENRTPT, HOENTPT
#' Dependency datasets: dm, se
#'
#' @param term Values to be mapped under HOTERM variable
#' @param prob Numeric vector - Probability of the event occurance. If the value is 1 for a event, then the event would be populated for all the subjects in DM dataset. If TERM is included but PROB is not included, 1 will be assigned as PROB.
#' @param cat Values to be mapped under HOCAT variable (optional)
#' @param scat Values to be mapped under HOSCAT variable (optional)
#' @param presp Values to be mapped under HOPRESP variable (optional) - The allowed input values are: N, NA, U, Y.
#' @param sort_seq Sorting sequence to be used for HOSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "HOTERM", "HOSTDTC", "HOENDTC"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be in UPPERCASE
#'
#' @return A data.frame with SDTM HO structure
#' @export
#'
#' @examples
#' \dontrun{
#' ho <- rho() #Creates HO dataset with predefined default values
#'
#' ho <- rho(term = c("GENERAL WARD", "HOSPITAL", "INTENSIVE CARE", "NURSING HOME"),
#'           cat = c("HOSPITALIZATION", "HOSPITALIZATION", "HOSPITALIZATION", NA),
#'           presp = c("Y", "Y", NA, NA),
#'           sort_seq = c("STUDYID", "USUBJID", "HOTERM", "HOSTDTC", "HOENDTC"))
#' }

rho <- function(term = c("GENERAL WARD", "HOSPITAL", "INTENSIVE CARE", "NURSING HOME"),
                prob = c(0.8, 0.5, 0.2, 0.2),
                cat = c("HOSPITALIZATION", "HOSPITALIZATION", "HOSPITALIZATION", NA),
                scat = c(),
                presp = c("Y", "Y", NA, NA),
                sort_seq = c("STUDYID", "USUBJID", "HOTERM", "HOSTDTC", "HOENDTC"),
                drop_vars = c()) {

  # Metadata for the HO dataset
  ho_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "HOSEQ" = "Sequence Number",
                      "HOGRPID" = "Group ID",
                      "HOREFID" = "Reference ID",
                      "HOSPID" = "Sponsor-Defined Identifier",
                      "HOTERM" = "Healthcare Encounter Term",
                      "HODECOD" = "Dictionary-Derived Term",
                      "HOCAT" = "Category for Healthcare Encounter",
                      "HOSCAT" = "Subcategory for Healthcare Encounter",
                      "HOPRESP" = "Pre-Specified Healthcare Encounter",
                      "HOOCCUR" = "Healthcare Encounter Occurrence",
                      "HOSTAT" = "Completion Status",
                      "HOREASND" = "Reason Healthcare Encounter Not Done",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "HODTC" = "Date/Time of Event Collection",
                      "HOSTDTC" = "Start Date/Time of Healthcare Encounter",
                      "HOENDTC" = "End Date/Time of Healthcare Encounter",
                      "HODY" = "Study Day of Event Collection",
                      "HOSTDY" = "Study Day of Start of Encounter",
                      "HOENDY" = "Study Day of End of Healthcare Encounter",
                      "HODUR" = "Duration of Healthcare Encounter",
                      "HOSTRTPT" = "Start Relative to Reference Time Point",
                      "HOSTTPT" = "Start Reference Time Point",
                      "HOENRTPT" = "End Relative to Reference Time Point",
                      "HOENTPT" = "End Reference Time Point"
                      )

  # Checking the availability of the dependent datasets
  if (!exists("dm") | !exists("se")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, se.")
  }

  # Checking if the required inputs have been provided
  if (is.null(term) | is.null(prob)) {
    stop("Error: Empty inputs given for one or more of the following parameters: term, prob. Make sure to add values OR remove the parameter to use default values.")
  }

  # Checking and restricting duplicate input for TERM
  if (any(duplicated(term))) {
    stop("Error: Duplicate values found as input for TERM. Make sure to inlucde unique values.")
  }

  # Check if input for PROB is provided with numeric values
  if (!is.numeric(prob)) {
    stop("Error: PROB should be provided with numeric values")
  }

  # NA values are not allowed as input for PROB
  if (is.numeric(prob) & any(is.na(prob))) {
    stop("Error: NA values ae not allowed as input for PROB. Input values should between 0.1 to 1.")
  }

  # PROB values should be between 0.1 and 1
  if (is.numeric(prob) & !all(prob >= 0.1 & prob <= 1)) {
    stop("Error: Input for PROB should between 0.1 to 1.")
  }

  # Checking when TERM is provided but optional parameters (which has default values) are not provided, in that case default values for the optional parameteres will be converted to NAs.
  if (!missing(term) & missing(cat)) {
    cat <- rep(NA, length(term))
  } else if (!missing(cat)) {
    cat <- opt_var_chk(opt_var = "cat", comp_var = "term")  # Comparing CAT and TERM inputs
  }

  if (!missing(term) & missing(presp)) {
    presp <- rep(NA, length(term))
  } else if (!missing(presp)) {
    presp <- opt_var_chk(opt_var = "presp", comp_var = "term")  # Comparing PRESP and TERM inputs
  }

  # If TERM is included but PROB is not included, assigning default value as 1 for PROB
  if (!missing(term) & missing(prob)) {
    prob <- rep(1, length(term))
  } else if (!missing(prob)) {
    if (length(prob) > 0 & length(prob) != length(term)) {
      stop("Error: No. of PROB and TERM provided not matched. Add PROB values for all the TERM values respevtively.")
    }
  }

  # Checking if PRESP is provided with allowed values
  if (!missing(presp) & !all(presp %in% c("N", "NA", "U", "Y", NA))) {
    stop("Error: Invalid input for presp. The allowed values are: N, NA, U, Y")
  }

  # Checking if the optional parameters (without default values) has necessary input values or else creating dummy values
  scat <- opt_var_chk(opt_var = "scat", comp_var = "term")  # Comparing SCAT and TERM inputs

  # Datafrane for term values
  term_df <- data.frame(term, cat, scat, presp, prob)

  # Crossing with dm dataframe to get records for all the subjects
  df1 <- crossing(dm %>% select(USUBJID, RFSTDTC), term_df)

  df_list <- list()

  # Retaining random subject records for each TERM based on the probability input
  for (term_in in df1$term) {

    df_temp1 <- df1 %>%
               filter(term == term_in)

    prob_1 <- unique(df_temp1$prob)

    df_temp2 <- df_temp1 %>%
                mutate(n_sample = with_seed(get_with_seed(), sample(c("Y", NA), size = n(), replace = TRUE, prob = c(prob_1, 1 - prob_1))))

    df_list[[term_in]] <- df_temp2

  }

  df2 <- bind_rows(df_list) %>%
         filter(n_sample == "Y")

  # Mapping general variables
  df3 <- df2 %>%
         mutate(STUDYID = get_studyid(),
                DOMAIN = "HO",
                HOTERM = term,
                HOCAT = cat,
                HOSCAT = scat,
                HOPRESP = presp,
                HOOCCUR = ifelse(presp == "Y", with_seed(get_with_seed(), sample(c("Y", "N"), size = n(), replace = TRUE, prob = c(0.9, 0.1))), NA),
                HOSTDTC = if_else(HOOCCUR != "N" | is.na(HOOCCUR), as.Date(as.Date(RFSTDTC) + with_seed(get_with_seed(), sample(1:20, size = n(), replace = TRUE))), as.Date(NA)),
                HOENDTC = if_else(HOOCCUR != "N" | is.na(HOOCCUR), as.Date(as.Date(HOSTDTC) + with_seed(get_with_seed(), sample(1:10, size = n(), replace = TRUE))), as.Date(NA))) %>%
                select(-RFSTDTC)

  # Mapping EPOCH variable
  df4 <- epoch(df = df3, dtc = "HOSTDTC")

  # Mapping --DY variable
  df5 <- stdy(df = df4, dtc = "HOSTDTC")
  df6 <- stdy(df = df5, dtc = "HOENDTC")

  # Mapping SEQ variable
  df7 <- seqnum(df = df6, sort = sort_seq)

  # Keeping only the Necessary variables
  df8 <- df7 %>%
         select(STUDYID, DOMAIN, USUBJID, HOSEQ, HOTERM, HOCAT, HOSCAT, HOPRESP, HOOCCUR,
                EPOCH, HOSTDTC, HOENDTC, HOSTDY, HOENDY)

  # Adding labels to the variables
  ho <- apply_metadata(df8, ho_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    ho <- ho %>% select(-all_of(drop_vars))
  }

  # Final HO dataset
  return(ho)
}
