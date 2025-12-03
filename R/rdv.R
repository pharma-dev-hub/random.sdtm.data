# R/rdv.R
#' Create Random Protocol Deviations (DV) Dataset
#'
#' @description
#' Creates a random SDTM DV (Protocol Deviations) dataset following CDISC SDTM standards.
#' Events. One record per protocol deviation per subject, Tabulation.
#' The list of pre-defined Deviation terms would be randomly mapped to random subjects from DM dataset.
#' The following Permissible variables have NOT been mapped within the rdv function: DVREFID, DVSPID, DVSCAT, TAETORD
#' Dependency datasets: dm, se
#'
#' @param domain By default, value has been set as "DV", user can modify it if needed but not recommended
#' @param sort_seq Sorting sequence to be used for DVSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "DVTERM", "DVDECOD", "DVCAT", "DVSTDTC", "DVENDTC"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be in UPPERCASE
#'
#' @return A data.frame with SDTM DV structure
#' @export
#'
#' @examples
#' rdv()
#'

rdv <- function(domain = "DV",
                sort_seq = c("STUDYID", "USUBJID", "DVTERM", "DVDECOD", "DVCAT", "DVSTDTC", "DVENDTC"),
                drop_vars = c()) {

  # Metadata for the DV dataset
  dv_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "DVSEQ" = "Sequence Number",
                      "DVREFID" = "Reference ID",
                      "DVSPID" = "Sponsor-Defined Identifier",
                      "DVTERM" = "Protocol Deviation Term",
                      "DVDECOD" = "Protocol Deviation Coded Term",
                      "DVCAT" = "Category for Protocol Deviation",
                      "DVSCAT" = "Subcategory for Protocol Deviation",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "DVSTDTC" = "Start Date/Time of Deviation",
                      "DVENDTC" = "End Date/Time of Deviation",
                      "DVSTDY" = "Study Day of Start of Deviation Event",
                      "DVENDY" = "Study Day of End of Deviation Event")

  # Logic checks
  # Checking the availability of the dependent datasets
  if (!exists("dm") | !exists("se")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, se.")
  }

  # General Deviation terms to be used
  dv_df <- read_xlsx("inst/data/deviations.xlsx", .name_repair = "universal")

  # Taking Random Subjects from DM to assign Deviations
  samp_subs <- dm %>%
               filter(row_number() %in% as.vector(with_seed(seed, sample(1:n(), size = floor(n()/2), replace = FALSE)))) %>%
               select(USUBJID, RFICDTC)

  # Crossing Subjects with Deviations and filtering random records
  df1 <- crossing(samp_subs, dv_df) %>%
         filter(row_number() %in% as.vector(with_seed(seed, sample(1:n(), size = floor(n()/5), replace = FALSE))))

  # Mapping General Variables
  df2 <- df1 %>%
         mutate(STUDYID = studyid,
                DOMAIN = domain,
                DVSTDTC = as.Date(as.Date(RFICDTC) + with_seed(seed, sample(1:60, size = n(), replace = TRUE))),
                DVENDTC = as.Date(as.Date(DVSTDTC) + with_seed(seed, sample(0:3, size = n(), replace = TRUE))))

  # Mapping EPOCH variable
  df3 <- epoch(df = df2, dtc = "DVSTDTC")

  # Mapping --DY variable
  df4 <- stdy(df = df3, dtc = "DVSTDTC")
  df5 <- stdy(df = df4, dtc = "DVENDTC")

  # Mapping SEQ variable
  df6 <- seqnum(df = df5, sort = sort_seq)

  # Keeping only the Necessary variables
  df7 <- df6 %>%
         select(STUDYID, DOMAIN, USUBJID, DVSEQ, DVTERM, DVDECOD, DVCAT, EPOCH, DVSTDTC, DVENDTC, DVSTDY, DVENDY)

  # Adding labels to the variables
  df8 <- apply_metadata(df7, dv_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    df8 <- df8 %>% select(-all_of(drop_vars))
  }

  # Final DV dataset
  assign("dv", df8, envir = .GlobalEnv)
}
