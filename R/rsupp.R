#' Create Random SUPP Dataset (Value-Level Metadata Compliant)
#'
#' @description
#' Generates a synthetic SDTM SUPP dataset using relevant data and value-level metadata.
#'
#' @param domain_data A data.frame representing the input dataset
#' @param seed Optional random seed for reproducibility
#'
#' @return A data.frame with SDTM SUPP structure
#' @export
#'
#' @examples
#' suppae <- rsuppae(ae)

rsupp <- function(domain_data, supp_terms, domain = "AE", idvar = "SEQ", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  checkmate::assert_data_frame(domain_data)
  checkmate::assert_data_frame(supp_terms)

  # Construct full IDVAR name (e.g., AESEQ, CMSEQ)
  idvar_full <- paste0(domain, idvar)

  # Add STUDYID and IDVAR columns to domain data
  domain_data <- domain_data %>%
    dplyr::mutate(
      STUDYID = "AD2025",  # Replace with actual study ID if needed
      IDVAR = idvar_full
    )

  # Sample metadata rows equal to domain_data rows
  sampled_terms <- supp_terms %>%
    dplyr::sample_n(nrow(domain_data), replace = TRUE)

  # Combine domain data with sampled metadata
  supp <- domain_data %>%
    dplyr::bind_cols(sampled_terms %>% dplyr::select(QNAM, QLABEL, QORIG, QEVAL)) %>%
    dplyr::mutate(
      RDOMAIN   = domain,
      IDVARVAL  = as.character(.data[[idvar_full]]),
      QVAL      = sample(c("Yes", "No", "Unknown"), n(), replace = TRUE)
    ) %>%
    dplyr::select(
      STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL,
      QNAM, QLABEL, QVAL, QORIG, QEVAL
    )

  # Apply metadata labels
  supp <- apply_metadata(supp, list(
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

  return(supp)
}

suppae <- rsupp(ae, suppae_terms, domain = "AE", idvar = "SEQ")
suppcm <- rsupp(cm, suppcm_terms, domain = "CM", idvar = "SEQ")
suppex <- rsupp(ex, suppex_terms, domain = "EX", idvar = "SEQ")
