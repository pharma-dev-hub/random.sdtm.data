# R/rte.R
#' Create Random Trial Elements (TE) Dataset
#'
#' @description
#' Creates a random SDTM TE (Trial Elements) dataset following CDISC SDTM standards.
#' Trial Design. One record per planned Element, Tabulation.
#'
#' @param domain By default, value has been set as "TE", user can modify it if needed but not recommended
#' @param etcd Values to be mapped under ETCD variable
#' @param element Values to be mapped under ELEMENT variable
#' @param testrl Values to be mapped under TESTRL variable
#' @param teenrl Values to be mapped under TEENRL variable (optional) - If TEENRL is included, add NA values in the input vector if any of the ELEMENT doesn't require TEENRL.
#' @param tedur Values to be mapped under TEDUR variable (optional) - If TEDUR is included, add NA values in the input vector if any of the ELEMENT doesn't require TEDUR.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be listed in UPPERCASE
#'
#' @return A data.frame with SDTM TE structure
#' @export
#'
#' @examples
#' rte(etcd = c("SCR", "TRT", "FUP"),
#'     element = c("Screening", "Treatment", "Follow-up"),
#'     testrl = c("First visit", "First dose", "End of treatment"),
#'     teenrl = c("First dose", "End of treatment", "End of study"),
#'     tedur = c("14 days", "10 weeks", "4 weeks"),
#'     drop_vars = c())
#'

rte <- function(domain = "TE",
                etcd = c(),
                element = c(),
                testrl = c(),
                teenrl = c(),
                tedur = c(),
                drop_vars = c()) {

  # Metadata for the TE dataset
  te_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "ETCD" = "Element Code",
                      "ELEMENT" = "Description of Element",
                      "TESTRL" = "Rule for Start of Element",
                      "TEENRL" = "Rule for End of Element",
                      "TEDUR" = "Planned Duration of Element")

  # Logic checks
  # Checking if all the required inputs have been provided
  if (missing(etcd) | missing(element) | missing(testrl)) {
    stop("Error: One or more Req variable input has NOT been provided. Make sure to add inputs for all the following parameters: etcd, element, testrl.")
  }

  # No. of ELEMENT/ETCD provided should be matched
  if (length(element) != length(etcd)) {
    stop("Error: No. of ELEMENT and ETCD provided not matched")
  }

  # No. of ELEMENT/TESTRL provided should be matched
  if (length(element) != length(testrl)) {
    stop("Error: No. of ELEMENT and TESTRL provided not matched")
  }

  # If TEENRL parameter included, then No. of ELEMENT/TEENRL provided should be matched
  if (length(teenrl) > 0 & length(teenrl) != length(element)) {
    stop("Error: No. of ELEMENT and TEENRL provided not matched. Add NA values if any of the ELEMENT doesn't require TEENRL.")
  }

  # Creation of dummy value for TEENRL, if TEENRL parameter is not included
  if (length(teenrl) == 0) {
    teenrl <- c(rep(NA, length(element)))
  }

  # If TEDUR parameter included, then No. of ELEMENT/TEDUR provided should be matched
  if (length(tedur) > 0 & length(tedur) != length(element)) {
    stop("Error: No. of ELEMENT and TEDUR provided not matched. Add NA values if any of the ELEMENT doesn't require TEDUR.")
  }

  # Creation of dummy value for TEDUR, if TEDUR parameter is not included
  if (length(tedur) == 0) {
    tedur <- c(rep(NA, length(element)))
  }

  # Combining all the variables as a dataframe
  evars <- data.frame(etcd, element, testrl, teenrl, tedur)

  # Naming all the variables in dataframe as in Uppercase
  names(evars) <- toupper(names(evars))

  df1 <- evars %>%
    mutate(STUDYID = studyid,
           DOMAIN = domain) %>%
    select(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR)

  # Adding labels to the variables
  df2 <- apply_metadata(df1, te_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    df2 <- df2 %>% select(-all_of(drop_vars))
  }

  # Final TE dataset
  assign("te", df2, envir = .GlobalEnv)
}
