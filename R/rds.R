# R/rds.R
#' Create Random Disposition (DS) Dataset
#'
#' @description
#' Creates a random SDTM DS (Disposition) dataset following CDISC SDTM standards.
#' Events. One record per disposition status or protocol milestone per subject, Tabulation.
#' The Subjects who have RFICDTC in DM are considered as "Informed Consent Obtained" Subjects.
#' The Subjects who have DTHFL in DM are considered as "Death" Subjects.
#' The Subjects who have RFPENDTC in DM are considered as "Completed" Subjects.
#' The following Permissible variables have NOT been mapped within the rds function: DSGRPID, DSREFID, DSSPID, DSSCAT, DSDTC, DSDY
#' Dependency datasets: dm, se
#'
#' @param domain By default, value has been set as "DS", user can modify it if needed but not recommended
#' @param sort_seq Sorting sequence to be used for DSSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "DSCAT", "DSDECOD", "DSSTDTC"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be in UPPERCASE
#'
#' @return A data.frame with SDTM DS structure
#' @export
#'
#' @examples
#' rds()
#'

rds <- function(domain = "DS",
                sort_seq = c("STUDYID", "USUBJID", "DSCAT", "DSDECOD", "DSSTDTC"),
                drop_vars = c()){

  # Metadata for the DS dataset
  ds_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "DSSEQ" = "Sequence Number",
                      "DSGRPID" = "Group ID",
                      "DSREFID" = "Reference ID",
                      "DSSPID" = "Sponsor-Defined Identifier",
                      "DSTERM" = "Reported Term for the Disposition Event",
                      "DSDECOD" = "Standardized Disposition Term",
                      "DSCAT" = "Category for Disposition Event",
                      "DSSCAT" = "Subcategory for Disposition Event",
                      "EPOCH" = "Epoch",
                      "DSDTC" = "Date/Time of Collection",
                      "DSSTDTC" = "Start Date/Time of Disposition Event",
                      "DSDY" = "Study Day of Collection",
                      "DSSTDY" = "Study Day of Start of Disposition Event")

  # Logic checks
  # Checking the availability of the dependent datasets
  if (!exists("dm") | !exists("se")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, se.")
  }

  # Protocol Milestone - Informed Consent Obtained Subjects
  ic_df <- dm %>%
           filter(!is.na(RFICDTC)) %>%
           mutate(DSTERM = "INFORMED CONSENT OBTAINED",
                  DSDECOD = "INFORMED CONSENT OBTAINED",
                  DSCAT = "PROTOCOL MILESTONE",
                  DSSTDTC = RFICDTC)

  # Disposition Event - Death Subjects
  dth_df <- dm %>%
            filter(DTHFL == "Y") %>%
            mutate(DSTERM = "DEATH",
                   DSDECOD = "DEATH",
                   DSCAT = "DISPOSITION EVENT",
                   DSSTDTC = DTHDTC)

  # Disposition Event - Completed Subjects
  ds_df <- dm %>%
           filter(!is.na(RFPENDTC) & (is.na(DTHFL) | DTHFL != "Y")) %>%
           mutate(DSTERM = "COMPLETED",
                  DSDECOD = "COMPLETED",
                  DSCAT = "DISPOSITION EVENT",
                  DSSTDTC = RFPENDTC)

  # Combining all the Disposition data
  df1 <- bind_rows(ic_df, dth_df, ds_df) %>%
         select(USUBJID, DSTERM, DSDECOD, DSCAT, DSSTDTC) %>%
         mutate(STUDYID = studyid,
                DOMAIN = domain)

  # Mapping EPOCH variable
  df2 <- epoch(df = df1, dtc = "DSSTDTC")

  # Mapping --DY variable
  df3 <- stdy(df = df2, dtc = "DSSTDTC")

  # Mapping SEQ variable
  df4 <- seqnum(df = df3, sort = sort_seq)

  # Keeping only the Necessary variables
  df5 <- df4 %>%
         select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC, DSSTDY)

  # Adding labels to the variables
  df6 <- apply_metadata(df5, ds_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    df6 <- df6 %>% select(-all_of(drop_vars))
  }

  # Final DS dataset
  assign("ds", df6, envir = .GlobalEnv)
}
