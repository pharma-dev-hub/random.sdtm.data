# R/rta.R
#' Create Random Trial Arms (TA) Dataset
#'
#' @description
#' Creates a random SDTM TA (Trial Arms) dataset following CDISC SDTM standards.
#' Trial Design. One record per planned Element per Arm, Tabulation.
#'
#' @param domain By default, value has been set as "TA", user can modify it if needed but not recommended
#' @param armcd Values to be mapped under ARMCD variable - Repeat the ARMCD values based on no.of. Elements within each ARM
#' @param arm Values to be mapped under ARM variable - Repeat the ARM values based on no.of. Elements within each ARM
#' @param taetord Numeric vector - Values to be mapped under TAETORD variable
#' @param etcd Values to be mapped under ETCD variable
#' @param element Values to be mapped under ELEMENT variable (optional) - If ellement() parameter is not included, NA values will be populated
#' @param tabranch Values to be mapped under TABRANCH  variable (optional) - Expected format: ARMCD|TAETORD|TABRANCH text. Each value must be separated by a pipe (|). ARMCD and TAETORD values should match respective inputs.
#' @param tatrans Values to be mapped under TATRANS  variable (optional) - Expected format: ARMCD|TAETORD|TATRANS text. Each value must be separated by a pipe (|). ARMCD and TAETORD values should match respective inputs.
#' @param epoch Values to be mapped under EPOCH  variable
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be listed in UPPERCASE
#'
#' @return A data.frame with SDTM TA structure
#' @export
#'
#' @examples
#' rta(armcd = c("ADA", "ADA", "ADA", "DER", "DER", "DER", "PLA", "PLA", "PLA"),
#'     arm =  c("Adalimumab", "Adalimumab", "Adalimumab", "Dermavalimab", "Dermavalimab", "Dermavalimab", "Placebo", "Placebo", "Placebo"),
#'     taetord = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'     etcd = c("SCR", "TRT", "FUP", "SCR", "TRT", "FUP", "SCR", "TRT", "FUP"),
#'     element = c("Screening", "Treatment", "Follow-up", "Screening", "Treatment", "Follow-up", "Screening", "Treatment", "Follow-up"),
#'     tabranch = c("ADA|1|Randomized to ADA","PLA|1|Randomized to PLA"),
#'     tatrans = c("DER|2|Dermavalimab Arm", "PLA|3|Placebo Arm"),
#'     epoch = c("Screening", "Treatment", "Follow-up", "Screening", "Treatment", "Follow-up", "Screening", "Treatment", "Follow-up"),
#'     drop_vars = c())
#'

rta <- function(domain = "TA",
                armcd = c(),
                arm = c(),
                taetord = c(),
                etcd = c(),
                element = c(),
                tabranch = c(),
                tatrans = c(),
                epoch = c(),
                drop_vars = c()) {

  # Metadata for the TA dataset
  ta_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "ARMCD" = "Planned Arm Code",
                      "ARM" = "Description of Planned Arm",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "ETCD" = "Element Code",
                      "ELEMENT" = "Description of Element",
                      "TABRANCH" = "Branch",
                      "TATRANS" = "Transition Rule",
                      "EPOCH" = "Epoch")

  # Logic checks
  # Checking if all the required inputs have been provided
  if (missing(armcd) | missing(arm) | missing(etcd) | missing(taetord) | missing(epoch)) {
    stop("Error: One or more Req variable input has NOT been provided. Make sure to add inputs for all the following parameters: armcd, arm, etcd, taetord, epoch.")
  }

  # No. of Arms/Elements provided should be matched
  length_chk <- c(length(armcd), length(arm), length(taetord), length(etcd), length(epoch))

  if (length(unique(length_chk)) != 1) {
    stop("Error: No. of Arms/Elements provided should be matched. No. of values provided for the following variables should be equal: arncd, arn, taetotd, etcd, epoch.")
  }

  # If ELEMENT parameter included, then No. of ELEMENT and No. of ETCD provided should be matched
  if (length(element) > 0 & length(element) != length(etcd)) {
    stop("Error: No. of ELEMENT and ETCD provided not matched")
  }

  # Creation of dummy value for ELEMENT, if ELEMENT parameter is not included
  if (length(element) == 0) {
    element <- c(rep(NA, length(etcd)))
  }

  # Check if input for TAETORD is provided with numeric values
  if (!is.numeric(taetord)) {
    stop("Error: TAETORD should be provided wirh numeric values")
  }

  # Checking if necessary inputs are provided for TABRANCH
  if (!is.null(tabranch) & length(tabranch) > 0) {

    tabranch_df <- as.data.frame(tabranch) %>%
                   mutate(ARMCD = sapply(strsplit(as.character(tabranch), "\\|"), "[", 1),
                          TAETORD = as.numeric(sapply(strsplit(as.character(tabranch), "\\|"), "[", 2)),
                          TABRANCH = sapply(strsplit(as.character(tabranch), "\\|"), "[", 3))

    if (!(all(tabranch_df$ARMCD %in% armcd)) | !(all(tabranch_df$TAETORD %in% taetord))){
      stop("Error: Invalid input for TABRANCH. Expected format: ARMCD|TAETORD|TABRANCH text. Each value must be separated by a pipe (|). Check if the values match the provided ARMCD and TAETORD inputs.")
    }
    # Creating empty TABRANCH value if no input has been provided
  } else {
    tabranch_df <- data.frame(ARMCD = character(), TAETORD = numeric(), TABRANCH = character())
  }

  # Checking if necessary inputs are provided for TATRANS
  if (!is.null(tatrans) & length(tatrans) > 0) {

    tatrans_df <- as.data.frame(tatrans) %>%
                  mutate(ARMCD = sapply(strsplit(as.character(tatrans), "\\|"), "[", 1),
                         TAETORD = as.numeric(sapply(strsplit(as.character(tatrans), "\\|"), "[", 2)),
                         TATRANS = sapply(strsplit(as.character(tatrans), "\\|"), "[", 3))

    if (!(all(tatrans_df$ARMCD %in% armcd)) | !(all(tatrans_df$TAETORD %in% taetord))){
      stop("Error: Invalid input for TATRANS. Expected format: ARMCD|TAETORD|TATRANS text. Each value must be separated by a pipe (|). Check if the values match the provided ARMCD and TAETORD inputs.")
    }
    # Creating empty TATRANS value if no input has been provided
  } else {
    tatrans_df <- data.frame(ARMCD = character(), TAETORD = numeric(), TATRANS = character())
  }

  # Combining all the variables as a dataframe
  arm_x_evars <- data.frame(arm, armcd, etcd, element, taetord, epoch)

  # Naming all the variables in dataframe as in Uppercase
  names(arm_x_evars) <- toupper(names(arm_x_evars))

  # Joining TABRANCH & TATRANS variables; Mapping STUDYID and DOMAIN
  df1 <- arm_x_evars %>%
         left_join(tabranch_df %>% select(ARMCD, TAETORD, TABRANCH), by = c("ARMCD" = "ARMCD", "TAETORD" = "TAETORD")) %>%
         left_join(tatrans_df %>% select(ARMCD, TAETORD, TATRANS), by = c("ARMCD" = "ARMCD", "TAETORD" = "TAETORD")) %>%
         mutate(STUDYID = studyid,
                DOMAIN = domain) %>%
         arrange(STUDYID, ARMCD, TAETORD) %>%
         select(STUDYID, DOMAIN, ARMCD, ARM, TAETORD, ETCD, ELEMENT, TABRANCH, TATRANS, EPOCH)

  # Adding labels to the variables
  df2 <- apply_metadata(df1, ta_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    df2 <- df2 %>% select(-all_of(drop_vars))
  }

  # Final TA dataset
  assign("ta", df2, envir = .GlobalEnv)
}
