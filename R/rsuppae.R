#' Create Random SUPPAE Dataset (Value-Level Metadata Compliant)
#'
#' @description
#' Generates a synthetic SDTM SUPPAE dataset using AE data and value-level metadata.
#'
#' @param ae A data.frame representing the AE dataset
#' @param seed Optional random seed for reproducibility
#'
#' @return A data.frame with SDTM SUPPAE structure
#' @export
#'
#' @examples
#' suppae <- rsuppae(ae)

rsuppae <- function(ae, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  checkmate::assert_data_frame(ae)

  # Controlled values from value-level metadata
  study_id <- "AD2025"
  rdomain <- "AE"
  idvar <- "AESEQ"
  qnam <- "AETRTREL"
  qlabel <- "AE Treatment Related"
  qval_options <- c("Yes", "No", "Unknown")
  qorig <- "CRF"
  qeval <- "Investigator"

  # Generate one SUPPAE record per AE record
  suppae <- ae %>%
    dplyr::mutate(
      STUDYID   = study_id,
      RDOMAIN   = rdomain,
      IDVAR     = idvar,
      IDVARVAL  = as.character(AESEQ),
      QNAM      = qnam,
      QLABEL    = qlabel,
      QVAL      = sample(qval_options, n(), replace = TRUE),
      QORIG     = qorig,
      QEVAL     = qeval
    ) %>%
    dplyr::select(
      STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL,
      QNAM, QLABEL, QVAL, QORIG, QEVAL
    )

  suppae <- apply_metadata(suppae, list(
    STUDYID   = "Study Identifier",
    RDOMAIN   = "Related Domain Abbreviation",
    USUBJID   = "Unique Subject Identifier",
    IDVAR     = "Identifying Variable",
    IDVARVAL  = "Identifying Variable Value",
    QNAM      = "Qualifier Variable Name",
    QLABEL    = "Qualifier Variable Label",
    QVAL      = "Data Value",
    QORIG     = "Origin",
    QEVAL     = "Evaluator"
  ))

  return(suppae)
}

suppae <- rsuppae(ae)


