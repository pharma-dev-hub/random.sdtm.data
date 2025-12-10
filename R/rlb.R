# R/rlb.R
#' Create Random Laboratory Test Results (LB) Dataset
#'
#' @description
#' Creates a random SDTM LB (Laboratory Test Results) dataset following CDISC SDTM standards.
#' Findings. One record per lab test per time point per visit per subject, Tabulation.
#' This function would create records with pre-defined Lab Test values for each Subject/Visits exists in SV Dataset.
#' User can drop unused Visit records based on CAT/SCAT/TESTCD or can drop entire VISIT/CAT/SCAT/TESTCD records.
#' The following Permissible variables have NOT been mapped within the rlb function: LBREFID, LBSPID, LBTSTCND, LBBDAGNT, LBTSTOPO, LBRESSCL, LBRESTYP, LBCOLSRT, LBLLOD, LBSTNRC, LBNAM, LBLOINC, LBSPCCND, LBSPCUFL, LBANMETH, LBTMTHSN, LBBLFL, LBFAST, LBDRVFL, LBTOX, LBTOXGR, LBCLSIG, VISITDY, TAETORD, LBENDTC, LBENDY, LBTPT, LBTPTNUM, LBELTM, LBTPTREF, LBRFTDTC, LBPTFL, LBPDUR
#' Dependency datasets: dm, se, sv
#'
#' @param drop_cat Mention the LBCAT value which needs to be dropped from the pre-defined list.
#' @param drop_scat Mention the LBSCAT value which needs to be dropped from the pre-defined list.
#' @param drop_testcd Mention the LBTESTCD value which needs to be dropped from the pre-defined list.
#' @param drop_visn Mention the VISITNUM value which needs to be dropped from the dataset.
#' @param drop_cat_visn Mention the VISITNUM values which needs to be dropped from the dataset for a particular LBCAT. Expected format: LBCAT|VISITNUM numeric values separated by comma. Example: "CHEMISTRY|1, 2, 3" - If VISITNUM 1, 2, 3 are not needed in dataset for "CHEMISTRY" LBCAT.
#' @param drop_scat_visn Mention the VISITNUM values which needs to be dropped from the dataset for a particular LBSCAT. Expected format: LBSCAT|VISITNUM numeric values separated by comma. Example: "Renal Function|1, 2, 3" - If VISITNUM 1, 2, 3 are not needed in dataset for "Renal Function" LBSCAT.
#' @param drop_testcd_visn Mention the VISITNUM values which needs to be dropped from the dataset for a particular LBTESTCD. Expected format: LBTESTCD|VISITNUM numeric values separated by comma. Example: "ALB|1, 2, 3" - If VISITNUM 1, 2, 3 are not needed in dataset for "ALB" LBTESTCD.
#' @param grpid Values to be mapped under LBGRPID variable - Expected format: TESTCD|VAR1|VAR2|...|VARn. Each value must be separated by a pipe (|). TESTCD values should match respective input. The values under VAR1, VAR2, … VARn would be concatenated separated by a hyphen (-). If same TESTCD value is given as input as more than once, the values for the TESTCDs will be duplicated when each time same TESTCD is added.
#' @param sort_seq Sorting sequence to be used for LBSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "LBCAT", "LBTESTCD", "LBTEST", "VISITNUM", "LBDTC"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped - Variabe names should be in UPPERCASE
#'
#' @return A data.frame with SDTM LB structure
#' @export
#'
#' @examples
#' \dontrun{
#' lb <- rlb()
#'
#' lb <- rlb(drop_cat = c("COAGULATION"),
#'           drop_testcd = c("ALB","ALT"),
#'           drop_visn = c(5, 7),
#'           drop_cat_visn = c("CHEMISTRY|1"),
#'           drop_scat_visn = c("Renal Function|3"),
#'           drop_testcd_visn = c("ALP|2, 4", "BILI|2, 4"),
#'           grpid = c("AST|USUBJID|LBTESTCD|LBSPEC", "BILI|USUBJID|LBTESTCD|LBSPEC|LBDTC"),
#'           sort_seq = c("STUDYID", "USUBJID", "VISITNUM", "LBTESTCD", "LBTEST", "LBDTC"))
#' }

rlb <- function(drop_cat = c(),
                drop_scat = c(),
                drop_testcd = c(),
                drop_visn = c(),
                drop_cat_visn = c(),
                drop_scat_visn = c(),
                drop_testcd_visn = c(),
                grpid = c(),
                sort_seq = c("STUDYID", "USUBJID", "LBCAT", "LBTESTCD", "LBTEST", "VISITNUM", "LBDTC"),
                drop_vars = c()) {

  # Metadata for the LB dataset
  lb_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "LBSEQ" = "Sequence Number",
                      "LBGRPID" = "Group ID",
                      "LBREFID" = "Specimen ID",
                      "LBSPID" = "Sponsor-Defined Identifier",
                      "LBTESTCD" = "Lab Test or Examination Short Name",
                      "LBTEST" = "Lab Test or Examination Name",
                      "LBTSTCND" = "Test Condition",
                      "LBBDAGNT" = "Binding Agent",
                      "LBTSTOPO" = "Test Operational Objective",
                      "LBCAT" = "Category for Lab Test",
                      "LBSCAT" = "Subcategory for Lab Test",
                      "LBORRES" = "Result or Finding in Original Units",
                      "LBORRESU" = "Original Units",
                      "LBRESSCL" = "Result Scale",
                      "LBRESTYP" = "Result Type",
                      "LBCOLSRT" = "Collected Summary Result Type",
                      "LBORNRLO" = "Reference Range Lower Limit in Orig Unit",
                      "LBORNRHI" = "Reference Range Upper Limit in Orig Unit",
                      "LBLLOD" = "Lower Limit of Detection",
                      "LBSTRESC" = "Character Result/Finding in Std Format",
                      "LBSTRESN" = "Numeric Result/Finding in Standard Units",
                      "LBSTRESU" = "Standard Units",
                      "LBSTNRLO" = "Reference Range Lower Limit-Std Units",
                      "LBSTNRHI" = "Reference Range Upper Limit-Std Units",
                      "LBSTNRC" = "Reference Range for Char Rslt-Std Units",
                      "LBNRIND" = "Reference Range Indicator",
                      "LBSTAT" = "Completion Status",
                      "LBREASND" = "Reason Test Not Done",
                      "LBNAM" = "Vendor Name",
                      "LBLOINC" = "LOINC Code",
                      "LBSPEC" = "Specimen Type",
                      "LBSPCCND" = "Specimen Condition",
                      "LBSPCUFL" = "Specimen Usability for the Test",
                      "LBMETHOD" = "Method of Test or Examination",
                      "LBANMETH" = "Analysis Method",
                      "LBTMTHSN" = "Test Method Sensitivity",
                      "LBLOBXFL" = "Last Observation Before Exposure Flag",
                      "LBBLFL" = "Baseline Flag",
                      "LBFAST" = "Fasting Status",
                      "LBDRVFL" = "Derived Flag",
                      "LBTOX" = "Toxicity",
                      "LBTOXGR" = "Standard Toxicity Grade",
                      "LBCLSIG" = "Clinically Significant, Collected",
                      "VISITNUM" = "Visit Number",
                      "VISIT" = "Visit Name",
                      "VISITDY" = "Planned Study Day of Visit",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "LBDTC" = "Date/Time of Specimen Collection",
                      "LBENDTC" = "End Date/Time of Specimen Collection",
                      "LBDY" = "Study Day of Specimen Collection",
                      "LBENDY" = "Study Day of End of Observation",
                      "LBTPT" = "Planned Time Point Name",
                      "LBTPTNUM" = "Planned Time Point Number",
                      "LBELTM" = "Planned Elapsed Time from Time Point Ref",
                      "LBTPTREF" = "Time Point Reference",
                      "LBRFTDTC" = "Date/Time of Reference Time Point",
                      "LBPTFL" = "Point in Time Flag",
                      "LBPDUR" = "Planned Duration")

  # Logic checks
  # Checking the availability of the dependent datasets
  if (!exists("dm") | !exists("se") | !exists("sv")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, se, sv.")
  }

  # Dataframe to drop specified Visitnum records for specified CAT
  if (!is.null(drop_cat_visn) & length(drop_cat_visn) > 0) {
    drop_cat_visn_in = drop_cat_visn
    drop_cat_visn_df <- as.data.frame(drop_cat_visn_in) %>%
                        mutate(cat = sapply(strsplit(as.character(drop_cat_visn_in), "\\|"), "[", 1),
                               cat_visn = sapply(strsplit(as.character(drop_cat_visn_in), "\\|"), "[", 2))
  } else {
    drop_cat_visn_df <- data.frame(cat = character(0), cat_visn = character(0))
  }

  # Dataframe to drop specified Visitnum records for specified SCAT
  if (!is.null(drop_scat_visn) & length(drop_scat_visn) > 0) {
    drop_scat_visn_in = drop_scat_visn
    drop_scat_visn_df <- as.data.frame(drop_scat_visn_in) %>%
                         mutate(scat = sapply(strsplit(as.character(drop_scat_visn_in), "\\|"), "[", 1),
                                scat_visn = sapply(strsplit(as.character(drop_scat_visn_in), "\\|"), "[", 2))
  } else {
    drop_scat_visn_df <- data.frame(scat = character(0), scat_visn = character(0))
  }

  # Dataframe to drop specified Visitnum records for specified TESTCD
  if (!is.null(drop_testcd_visn) & length(drop_testcd_visn) > 0) {
    drop_testcd_visn_in = drop_testcd_visn
    drop_testcd_visn_df <- as.data.frame(drop_testcd_visn_in) %>%
                           mutate(testcd = sapply(strsplit(as.character(drop_testcd_visn_in), "\\|"), "[", 1),
                                  testcd_visn = sapply(strsplit(as.character(drop_testcd_visn_in), "\\|"), "[", 2))
  } else {
    drop_testcd_visn_df <- data.frame(testcd = character(0), testcd_visn = character(0))
  }

  options(scipen = 999)  # Prevent scientific notation in display

  # Importing TEST, TESTCD, CAT, SCAT, RANGE Values as input
  path_lb <- system.file("extdata", "lb_input.xlsx", package = "random.sdtm.data", mustWork = TRUE)

  lb_in <- read_xlsx(path_lb, .name_repair = "universal")

  # Mapping --GRPID variables
  if (length(grpid) > 0) {

    grpid_df <- as.data.frame(grpid) %>%
                mutate(testcd = sapply(strsplit(as.character(grpid), "\\|"), "[", 1),
                       var_count = str_count(grpid, "\\|"),
                       grpid_vars = mapply(function(test_var, grp_var) gsub(paste0(test_var, "\\|"), "", grp_var), testcd, grpid))

    grpid_df1 <- left_join(grpid_df, lb_in, by = c("testcd" = "LBTESTCD"))

    # Checking if TESTCD values provided under GRPID mathces the actual TESTCD input
    if (!(all(grpid_df1$testcd %in% as.vector(unique(lb_in$LBTESTCD))))) {
      stop("Error: Invalid input for GRPID. Expected format: TESTCD|VAR1|VAR2|...|VARn. Each value must be separated by a pipe (|). Check if the values match the provided TESTCD input.")
    }

    lb_in <- left_join(lb_in, grpid_df %>% select(testcd, grpid_vars), by = c("LBTESTCD" = "testcd"))
  }

  # Input SV dataset to create data for each visit occurred
  sv_in <- sv %>%
           filter(SVOCCUR == "Y" | is.na(SVOCCUR) | SVOCCUR == "") %>%
           select(USUBJID, VISIT, VISITNUM, SVSTDTC)

  # Crossing SV with LB input to get test records for each subject/visit
  df1 <- crossing(sv_in, lb_in)

  # LBALL Records
  # Input SV dataset to create LBALL data for each visit not occurred
  sv_lball <- sv %>%
              filter(SVOCCUR == "N") %>%
              select(USUBJID, VISIT, VISITNUM, SVSTDTC)

  # Taking unique values from LB Input
  cat_unique <- lb_in %>% distinct(LBCAT, LBSCAT, LBSPEC)

  # Creating LBALL records
  lball_df <- crossing(cat_unique, sv_lball) %>%
              mutate(LBTESTCD = "LBALL",
                     LBTEST = "Laboratory Test Results")

  # Splitting data into two based on Result values type Numeric/Character
  # Numeric result Tests
  res_n <- df1 %>% filter(!is.na(min))

  # Character result Tests
  res_c <- df1 %>% filter(!is.na(QN_VALS))

  # Creating Random numeric result values
  res_n$result_n <- NA

  for (testcd_loop in unique(res_n$LBTESTCD)) {

    temp <- res_n[res_n$LBTESTCD == testcd_loop,]

    res_n$result_n[res_n$LBTESTCD == testcd_loop] <- rand_res(n = nrow(res_n[res_n$LBTESTCD == testcd_loop,]),
                                                              mean = temp$mean,
                                                              sd = temp$sd,
                                                              min = temp$min,
                                                              max = temp$max,
                                                              dec = temp$dec)
  }

  # Creating Random Character result values
  res_c$result_c <- NA

  for (testcd_loop_char in unique(res_c$LBTESTCD)) {

    temp <- res_c[res_c$LBTESTCD == testcd_loop_char,]

    QN_VALS_IN <- as.vector(strsplit(unique(temp$QN_VALS), ",")[[1]])

    res_c$result_c[res_c$LBTESTCD == testcd_loop_char] <- rand_res_char(choices = QN_VALS_IN,
                                                                        n = nrow(res_c[res_c$LBTESTCD == testcd_loop_char,]))
  }

  # Combining Numeric and Character result data with LBALL data
  df2 <- bind_rows(res_n, res_c, lball_df)

  # Mapping general variables
  df3 <- df2 %>%
         mutate(STUDYID = get_studyid(),
                DOMAIN = "LB",
                LBORRES = ifelse(!is.na(as.character(result_n)), as.character(result_n), as.character(result_c)),
                LBORRESU = ifelse(LBORRES != "" & !is.na(LBORRES), as.character(LBORRESU), NA_character_),
                LBSTRESN = ifelse(!is.na(conv_factor) & conv_factor != 1, conv_factor * suppressWarnings(as.numeric(LBORRES)), suppressWarnings(as.numeric(LBORRES))),
                LBSTRESC = ifelse(!is.na(as.character(LBSTRESN)), as.character(LBSTRESN), as.character(LBORRES)),
                LBSTRESU = ifelse(LBSTRESC != "" & !is.na(LBSTRESC), as.character(LBSTRESU), NA_character_),
                LBORNRLO = as.character(LBORNRLO),
                LBORNRHI = as.character(LBORNRHI),
                LBSTNRLO = as.numeric(LBSTNRLO),
                LBSTNRHI = as.numeric(LBSTNRHI),
                LBNRIND = case_when(!is.na(LBSTRESN) & !is.na(LBSTNRLO) & as.numeric(LBSTRESN) < as.numeric(LBSTNRLO) ~ "LOW",
                                    !is.na(LBSTRESN) & !is.na(LBSTNRHI) & as.numeric(LBSTRESN) > as.numeric(LBSTNRHI) ~ "HIGH",
                                    !is.na(LBSTRESN) & !is.na(LBSTNRLO) & !is.na(LBSTNRHI) & as.numeric(LBSTNRLO) <= as.numeric(LBSTRESN) & as.numeric(LBSTRESN) <= as.numeric(LBSTNRHI) ~ "NORMAL",
                                    !is.na(LBSTRESN) & !is.na(LBSTNRLO) & is.na(LBSTNRHI) & as.numeric(LBSTRESN) >= as.numeric(LBSTNRLO) ~ "NORMAL",
                                    !is.na(LBSTRESN) & is.na(LBSTNRLO) & !is.na(LBSTNRHI) & as.numeric(LBSTRESN) <= as.numeric(LBSTNRHI) ~ "NORMAL",
                                    TRUE ~ NA_character_),
                LBSTAT = ifelse(is.na(LBORRES), "NOT DONE", NA_character_),
                LBREASND = ifelse(is.na(LBORRES), "UNKNOWN", NA_character_),
                LBDTC = SVSTDTC,
                VISITNUM = as.numeric(VISITNUM)) %>%
         # Dropping records as per input
         left_join(drop_cat_visn_df, by = c("LBCAT" = "cat")) %>%
         left_join(drop_scat_visn_df, by = c("LBSCAT" = "scat")) %>%
         left_join(drop_testcd_visn_df, by = c("LBTESTCD" = "testcd")) %>%
         filter(!LBCAT %in% drop_cat) %>%
         filter(!LBSCAT %in% drop_scat) %>%
         filter(!LBTESTCD %in% drop_testcd) %>%
         filter(!VISITNUM %in% drop_visn) %>%
         rowwise() %>%
         filter(!VISITNUM %in% as.numeric(strsplit(as.character(cat_visn), ",")[[1]])) %>%
         filter(!VISITNUM %in% as.numeric(strsplit(as.character(scat_visn), ",")[[1]])) %>%
         filter(!VISITNUM %in% as.numeric(strsplit(as.character(testcd_visn), ",")[[1]])) %>%
         ungroup()

  # Mapping LBGRPID Variable if input is provided
  if (length(grpid) > 0 ) {

    # Checking if input variables are available
    vars <- strsplit(grpid, "\\|")

    for (rows in seq_along(vars)){
      vars_row <- vars[[rows]]

      for (var in seq_along(vars_row)){

        if (var > 1) {   #Ignoring the TESTCD value

          if (!(vars_row[var]  %in% names(df3))) {
            stop("Error: Variable used to group is not an valid variable: ", vars_row[var])
          }
        }
      }
    }

    # Mapping --GRPID Variable separated by hyphen (-)
    df4 <- df3 %>%
           mutate(LBGRPID = pmap_chr(., function(..., grpid_vars) {   # pmap_chr() applies a function to each row
             row <- list(...)                  # row <- list(...) captures all columns in the current row.
            if (!is.na(grpid_vars)) {       # unlist(row[valid_cols]) gets the values to concatenate.
              cols <- str_split(grpid_vars, "\\|")[[1]]
              valid_cols <- cols[cols %in% names(row)]
              if (length(valid_cols) > 0) {
                paste(unlist(row[valid_cols]), collapse = "-")
              } else {
                NA_character_
              }
            } else {
              NA_character_
            }
            }))
  } else {
    df4 <- df3 %>%
           mutate(LBGRPID = NA_character_)
  }

  # Mapping LOBXFL variable
  df5 <- lobxfl(df = df4, dtc = "LBDTC", res_var = "LBORRES", sort = c("USUBJID", "LBCAT", "LBSCAT", "LBSPEC", "LBTESTCD"))

  # Mapping EPOCH variable
  df6 <- epoch(df = df5, dtc = "LBDTC")

  # Mapping --DY variable
  df7 <- stdy(df = df6, dtc = "LBDTC")

  # Mapping SEQ variable
  df8 <- seqnum(df = df7, sort = sort_seq)

  # Keeping only the Necessary variables
  df9 <- df8 %>%
         select(STUDYID, DOMAIN, USUBJID, LBSEQ, LBGRPID, LBTESTCD, LBTEST, LBCAT, LBSCAT, LBORRES, LBORRESU,
                LBORNRLO, LBORNRHI, LBSTRESC, LBSTRESN, LBSTRESU, LBSTNRLO, LBSTNRHI, LBNRIND, LBSTAT, LBREASND,
                LBSPEC, LBMETHOD, LBLOBXFL, VISITNUM, VISIT, EPOCH, LBDTC, LBDY)

  # Adding labels to the variables
  lb <- apply_metadata(df9, lb_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    lb <- lb %>% select(-all_of(drop_vars))
  }

  # Final LB dataset
  return(lb)
}
