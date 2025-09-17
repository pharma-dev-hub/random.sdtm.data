# R/rtv.R
#' Create Random Trial Visits (TV) Dataset
#'
#' @description
#' Creates a random SDTM TV (Trial Visits) dataset following CDISC SDTM standards.
#' Trial Design. One record per planned Visit per Arm, Tabulation.
#'
#' @param domain By default, value has been set as "TV", user can modify it if needed but not recommended
#' @param visitnum Numeric vector - Values to be mapped under VISITNUM variable
#' @param visit Values to be mapped under VISIT variable
#' @param visitdy Numeric vector - Values to be mapped under VISITDY variable (optional)
#' @param armcd Values to be mapped under ARMCD variable (optional)
#' @param arm Values to be mapped under ARM variable (optional)
#' @param tvstrl Values to be mapped under TVSTRL variable
#' @param tvenrl Values to be mapped under TVENRL variable (optional)
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be listed in UPPERCASE
#'
#' @return A data.frame with SDTM TV structure
#' @export
#'
#' @examples
#' rtv(visitnum = c(1, 2, 3, 4, 5, 6, 7, 8),
#'     visit = c("Screening", "Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "Week 10", "Follow-up"),
#'     visitdy = c(-14, 1, 15, 29, 43, 57, 71, 99),
#'     armcd = c(),
#'     arm = c(),
#'     tvstrl = c("Based on scheduled calendar days from first dose", "Based on scheduled calendar days from first dose", "Based on scheduled calendar days from first dose", "Based on scheduled calendar days from first dose", "Based on scheduled calendar days from first dose", "Based on scheduled calendar days from first dose", "Based on scheduled calendar days from first dose", "Based on scheduled calendar days from first dose"),
#'     tvenrl = c(),
#'     drop_vars = c())
#'

rtv <- function(domain ="TV",
                visitnum = c(),
                visit = c(),
                visitdy = c(),
                armcd = c(),
                arm = c(),
                tvstrl = c(),
                tvenrl = c(),
                drop_vars = c()) {

  # Metadata for the TV dataset
  tv_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "VISITNUM" = "Visit Number",
                      "VISIT" = "Visit Name",
                      "VISITDY" = "Planned Study Day of Visit",
                      "ARMCD" = "Planned Arm Code",
                      "ARM" = "Description of Planned Arm",
                      "TVSTRL" = "Visit Start Rule",
                      "TVENRL" = "Visit End Rule")

  # Logic checks
  # Checking if all the required inputs have been provided
  if (missing(visitnum) | missing(visit) | missing(tvstrl)) {
    stop("Error: One or more Req variable input has NOT been provided. Make sure to add inputs for all the following parameters: visitnum, visit, tvstrl.")
  }

  # Check if input for VISITNUM is provided with numeric values
  if (!is.numeric(visitnum)) {
    stop("Error: VISITNUM should be provided wirh numeric values")
  }

  # Check if input for VISITDY is provided with numeric values
  if (length(visitdy) > 0 & !is.numeric(visitdy)) {
    stop("Error: VISITDY should be provided wirh numeric values")
  }

  # No. of Visit/Visitnum provided should be matched
  if (length(visitnum) != length(visit)) {
    stop("Error: No. of VISITNUM and VISIT provided not matched")
  }

  # No. of VISIT and TVSTRL provided should be matched
  if (length(visit) != length(tvstrl)) {
    stop("Error: No. of VISIT and TVSTRL provided not matched")
  }

  # No. of VISIT and VISITDY provided should be matched
  if (length(visitdy) > 0 & length(visit) != length(visitdy)) {
    stop("Error: No. of VISIT and VISITDY provided not matched. Add NA values if any of the VISIT doesn't require VISITDY.")
  }

  # Creation of dummy value for VISITDY, if VISITDY parameters are not included
  if (length(visitdy) == 0) {
    visitdy <- c(rep(NA, length(visit)))
  }

  # No. of VISIT/ARM/ARNCD provided should be matched
  if ((length(arm) > 0 & length(visit) != length(arm)) | (length(armcd) > 0 & length(visit) != length(armcd))) {
    stop("Error: No. of VISIT and ARM/ARMCD values provided not matched")
  }

  # Creation of dummy value for ARM/ARMCD, if ARM/ARMCD parameters are not included
  if (length(arm) == 0) {
    arm <- c(rep(NA, length(visit)))
  }

  if (length(armcd) == 0) {
    armcd <- c(rep(NA, length(visit)))
  }

  # If TVENRL is included No. of VISIT and TVENRL should be matched
  if (length(tvenrl) > 0 & length(tvenrl) != length(visit)) {
    stop("Error: No. of VISIT and TVENRL provided not matched. Add NA values if any of the VISIT doesn't require TVENRL.")
  }

  # Creation of dummy value for TVENRL, if TVENRL parameter is not included
  if (length(tvenrl) == 0) {
    tvenrl <- c(rep(NA, length(visit)))
  }

  # Combining all the variables as a dataframe
  visit_vars <- data.frame(visitnum, visit, visitdy, armcd, arm, tvstrl, tvenrl)

  # Naming all the variables in dataframe as in Uppercase
  names(visit_vars) <- toupper(names(visit_vars))

  df1 <- visit_vars %>%
         mutate(STUDYID = studyid,
                DOMAIN = domain) %>%
         arrange(ARMCD, VISITNUM, VISIT) %>%
         select(STUDYID, DOMAIN, VISITNUM, VISIT, VISITDY, ARMCD, ARM, TVSTRL, TVENRL)

  # Adding labels to the variables
  df2 <- apply_metadata(df1, tv_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    df2 <- df2 %>% select(-all_of(drop_vars))
  }

  # Final TV dataset
  assign("tv", df2, envir = .GlobalEnv)
}
