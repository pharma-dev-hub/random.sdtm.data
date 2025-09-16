# rti Function to create synthetic TI dataset
rti <- function(domain = "TI",
                studyid = "AD2025",
                ietestcd = c(),
                ietest = c(),
                iecat = c(),
                iescat = c(),
                tirl = c(),
                tivers = c(),
                drop_vars = c()){
 
  # Metadata for the TI dataset
  ti_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "IETESTCD" = "Incl/Excl Criterion Short Name",
                      "IETEST" = "Inclusion/Exclusion Criterion",
                      "IECAT" = "Inclusion/Exclusion Category",
                      "IESCAT" = "Inclusion/Exclusion Subcategory",
                      "TIRL" = "Inclusion/Exclusion Criterion Rule",
                      "TIVERS" = "Protocol Criteria Versions"
                      )
  
  # Logic checks
  # Checking if all the required inputs have been provided
  if (missing(ietestcd) | missing(ietest) | missing(iecat)) {
    stop("Error: One or more Req variable input has NOT been provided. Make sure to add inputs for all the following parameters: ietestcd, ietest, iecat.")
  }
  
  # No. of IETESTCD/IETEST provided should be matched
  if (length(ietestcd) != length(ietest)) {
    stop("Error: No. of IETESTCD and IETEST provided not matched")
  }
  
  # No. of IETESTCD/IECAT provided should be matched
  if (length(ietestcd) != length(iecat)) {
    stop("Error: No. of IETESTCD and IECAT provided not matched")
  }
  
  # If IESCAT parameter included, then No. of IETESTCD/IESCAT provided should be matched
  if (length(iescat) > 0 & length(iescat) != length(ietestcd)) {
    stop("Error: No. of IETESTCD and IESCAT provided not matched. Add NA values if any of the IETESTCD doesn't require IESCAT.")
  }
  
  # Creation of dummy value for IESCAT, if IESCAT parameter is not included
  if (length(iescat) == 0) {
    iescat <- c(rep(NA, length(ietestcd)))
  }
  
  # Checking if necessary inputs are provided for TIRL
  if (!is.null(tirl) & length(tirl) > 0) {
    
    tirl_df <- as.data.frame(tirl) %>% 
               mutate(IETESTCD = sapply(strsplit(tirl, "\\|"), "[", 1),
               TIRL = sapply(strsplit(tirl, "\\|"), "[", 2))
    
    if (!(all(tirl_df$IETESTCD %in% ietestcd))){
      stop("Error: Invalid input for TIRL. Expected format: IETESTCD|TIRL text. Both values must be separated by a pipe (|). Check if the values match the provided IETESTCD input.")
    }
    # Creating empty TIRL value if no input has been provided
  } else {
    tirl_df <- data.frame(IETESTCD = character(), TIRL = character())
  }
  
  # If TIVERS parameter included, then No. of IETESTCD/TIVERS provided should be matched
  if (length(tivers) > 0 & length(tivers) != length(ietestcd)) {
    stop("Error: No. of IETESTCD and TIVERS provided not matched. Add NA values if any of the IETESTCD doesn't require TIVERS.")
  }
  
  # Creation of dummy value for TIVERS, if TIVERS parameter is not included
  if (length(tivers) == 0) {
    tivers <- c(rep(NA, length(ietestcd)))
  }
  
  # Combining all the variables as a dataframe
  ti_vars <- data.frame(ietestcd, ietest, iecat, iescat, tivers)
  
  # Naming all the variables in dataframe as in Uppercase
  names(ti_vars) <- toupper(names(ti_vars))
  
  # Joining TIRL variable; Mapping STUDYID and DOMAIN
  df1 <- ti_vars %>% 
         left_join(tirl_df %>% select(IETESTCD, TIRL), by = c("IETESTCD" = "IETESTCD")) %>% 
         mutate(STUDYID = studyid,
                DOMAIN = domain) %>%
         arrange(STUDYID, TIVERS, desc(IECAT), IETESTCD) %>% 
         select(STUDYID, DOMAIN, IETESTCD, IETEST, IECAT, IESCAT, TIRL, TIVERS)
  
  # Adding labels to the variables
  df2 <- apply_metadata(df1, ti_metadata)
  
  # Drop Variables
  if (length(drop_vars) > 0) {
    df2 <- df2 %>% select(-all_of(drop_vars))
  }
  
  # Final TI dataset
  assign("ti", df2, envir = .GlobalEnv)
}

# Sample calling of rti function
rti(ietestcd = c("IN01", "IN02", "EX01", "EX02", "EX03", "IN01_V2", "IN02_V2", "EX01_V2", "EX02_V2", "EX03_V2"), 
    ietest = c("Inclusion Criteria 1", "Inclusion Criteria 2", "Exclusion Criteria 1", "Exclusion Criteria 2", "Exclusion Criteria 3", 
               "Inclusion Criteria 1 V2", "Inclusion Criteria 2 V2", "Exclusion Criteria 1 V2", "Exclusion Criteria 2 V2", "Exclusion Criteria 3 V2"),
    iecat = c("INCLUSION", "INCLUSION", "EXCLUSION", "EXCLUSION", "EXCLUSION", "INCLUSION", "INCLUSION", "EXCLUSION", "EXCLUSION", "EXCLUSION"),
    iescat = c("Minor", rep(NA, 9)),
    tirl = c("IN01|Inclusion 1 Rue", "EX02_V2|Rule for Exclusion 2"),
    tivers = c(rep("1.0", 5), rep("2.0", 5)),
    drop_vars = c()
    )
