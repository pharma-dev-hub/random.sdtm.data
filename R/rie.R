# R/rie.R
#' Create Random Inclusion/Exclusion Criteria Not Met (IE) Dataset
#'
#' @description
#' Creates a random SDTM IE (Inclusion/Exclusion Criteria Not Met) dataset following CDISC SDTM standards.
#' Findings. One record per inclusion/exclusion criterion not met per subject, Tabulation.
#' Function is designed to create IE dataset by including random Inclusion/Exclusion Cretria from TI, for the SCREEN FAILURE Subjects in DM dataset.
#' For each Subject, the first VISIT value from SV dataset would be used to map VISIT/VISITNUM values and respective SVSTDTC would be mapped to IEDTC.
#' The following Permissible variables have NOT been mapped within the rie function: IESPID, VISITDY, TAETORD.
#' Dependency datasets: ti, dm, se, sv
#'
#' @param sort_seq Sorting sequence to be used for IESEQ mapping. Default value is given as c("STUDYID", "USUBJID", "VISITNUM", "IETESTCD", "IETEST", "IEDTC"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be in UPPERCASE
#'
#' @return A data.frame with SDTM IE structure
#' @export
#'
#' @examples
#' \dontrun{
#' ie <- rie()
#' }

rie <- function(sort_seq = c("STUDYID", "USUBJID", "VISITNUM", "IETESTCD", "IETEST", "IEDTC"),
                drop_vars = c()) {

  # Metadata for the IE dataset
  ie_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "IESEQ" = "Sequence Number",
                      "IESPID" = "Sponsor-Defined Identifier",
                      "IETESTCD" = "Inclusion/Exclusion Criterion Short Name",
                      "IETEST" = "Inclusion/Exclusion Criterion",
                      "IECAT" = "Inclusion/Exclusion Category",
                      "IESCAT" = "Inclusion/Exclusion Subcategory",
                      "IEORRES" = "I/E Criterion Original Result",
                      "IESTRESC" = "I/E Criterion Result in Std Format",
                      "VISITNUM" = "Visit Number",
                      "VISIT" = "Visit Name",
                      "VISITDY" = "Planned Study Day of Visit",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "IEDTC" = "Date/Time of Collection",
                      "IEDY" = "Study Day of Collection")

  # Logic checks
  # Checking the availability of the dependent datasets
  if (!exists("ti") | !exists("dm") | !exists("se") | !exists("sv")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: ti, dm, se, sv.")
  }

  # Filtering Sreen Failure records from DM dataset
  dm_sf <- dm %>%
    filter(ARMNRS == "SCREEN FAILURE")

  # Creation of IE dataset with zero records if no subjects were "Screen Failure" in dm
  if (nrow(dm_sf) == 0) {

    # Creation of a dataframe from variables in metadata
    df1 <- data.frame(matrix(ncol = length(ie_metadata), nrow = 0))

    names(df1) <- names(ie_metadata)

    # Applying labels to the variables
    for (var in names(ie_metadata)) {
      if (var %in% names(df1)) {
        attr(df1[[var]], "label") <- ie_metadata[[var]]
      }
    }

    # Retaining only necessary variables
    ie <- df1 %>%
           select(STUDYID, DOMAIN, USUBJID, IESEQ, IETESTCD, IETEST, IECAT, IESCAT, IEORRES, IESTRESC, VISITNUM, VISIT, EPOCH, IEDTC, IEDY)

    # Drop Variables
    if (length(drop_vars) > 0) {
      ie <- ie %>% select(-all_of(drop_vars))
    }

    # IE dataset with zero observation
    return(ie)

    return("NOTE: No subjects were Screen Failure in dm dataset, and hence IE dataset has been created with zero observation")
  }

  # Mapping each subject into only one protocol version if more than one TIVERS available in TI dataset
  tivers <- data.frame(tivers_f = unique(ti$TIVERS))

  # Mapping unique TIVERS values to all the subjects
  sub1 <- crossing(dm_sf %>%  select(USUBJID), tivers)

  df_list <- list()

  # Random values to select TIVERS for each subject
  n_sample <- with_seed(get_with_seed(), sample(1:as.numeric(nrow(tivers)), size = length(unique(sub1$USUBJID)), replace = TRUE))

  itr <- 0

  # Keeping only one random TIVERS for each subjects
  for (subject in unique(sub1$USUBJID)) {

    itr <- itr + 1

    temp1 <- sub1 %>%
             filter(USUBJID == subject) %>%
             filter(row_number() == as.numeric(n_sample[itr]))

    df_list[[subject]] <- temp1
  }

  # Binding all the subjects data into a dataframe
  sub2 <- bind_rows(df_list)

  # SV dataset to be used as input
  sv_in <- sv %>%
           filter(!is.na(SVSTDTC)) %>%
           arrange(USUBJID, VISITNUM, SVSTDTC, SVENDTC) %>%
           group_by(USUBJID) %>%
           slice(1) %>%
           ungroup() %>%
           select(USUBJID, VISIT, VISITNUM, SVSTDTC)

  # Joining SV data to Subject data
  sub3 <- left_join(sub2, sv_in, by = "USUBJID")

  # Getting IETEST, IETESTCD, IECAT, IESCAT values from TI dataframe
  df1 <- crossing(sub3, ti %>% select(-STUDYID, -DOMAIN)) %>%
         filter(tivers_f == TIVERS | (is.na(tivers_f) & is.na(TIVERS)))

  n_sample_t <- list()

  itr_s <- 0

  # Sample values for each subject based on no.of. Records of each subject
  for(subject in unique(df1$USUBJID)) {

    itr_s <- itr_s + 1

    temp_df1 <- df1 %>% filter(USUBJID == subject)

    n_sample_t[itr_s] <- with_seed(get_with_seed() + itr_s, sample(1:as.numeric(nrow(temp_df1)), size = 1, replace = TRUE))
  }

  itr_t <- 0

  iedf_list <- list()

  # Keeping random Inclusion/Exclusion values for each subject
  for(subject in unique(df1$USUBJID)) {

    itr_t <- itr_t + 1

    temp_df1 <- df1 %>% filter(USUBJID == subject)

    temp_df2 <- temp_df1 %>%
                filter(row_number() %in% as.vector(with_seed(get_with_seed(), sample(1:n(), size = as.numeric(n_sample_t[[itr_t]]), replace = FALSE))))

    iedf_list[[subject]] <- temp_df2
  }

  # Binding all the subjects data into a dataframe
  df2 <- bind_rows(iedf_list)

  # Mapping general variables and Result variables
  df3 <- df2 %>%
         mutate(STUDYID = get_studyid(),
                DOMAIN = "IE",
                IEORRES = case_when(IECAT == "EXCLUSION" ~ "Y",
                                    IECAT == "INCLUSION" ~ "N",
                                    TRUE ~ NA_character_),
                IESTRESC = IEORRES,
                IEDTC = SVSTDTC) %>%
         select(-tivers_f, -SVSTDTC)

  # Mapping EPOCH variable
  df4 <- epoch(df = df3, dtc = "IEDTC")

  # Mapping --DY variable
  df5 <- stdy(df = df4, dtc = "IEDTC")

  # Mapping SEQ variable
  df6 <- seqnum(df = df5, sort = sort_seq)

  # Keeping only the Necessary variables
  df7 <- df6 %>%
         select(STUDYID, DOMAIN, USUBJID, IESEQ, IETESTCD, IETEST, IECAT, IESCAT, IEORRES, IESTRESC, VISITNUM, VISIT, EPOCH, IEDTC, IEDY)

  # Adding labels to the variables
  ie <- apply_metadata(df7, ie_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    ie <- ie %>% select(-all_of(drop_vars))
  }

  # Final IE dataset
  return(ie)
}
