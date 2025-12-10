# R/reg.R
#' Create Random ECG Test Results (EG) Dataset
#'
#' @description
#' Creates a random SDTM EG (ECG Test Results) dataset following CDISC SDTM standards.
#' Findings. One record per ECG observation per replicate per time point or one record per ECG observation per beat per visit per subject, Tabulation.
#' Function is designed to create values for only the following Tests: "ECG Mean Heart Rate", "Interpretation", "PR Interval, Aggregate", "QRS Duration, Aggregate", "QT Interval, Aggregate", "QTca Interval, Aggregate", "QTcB Interval, Aggregate", "QTcF Interval, Aggregate", "RR Interval, Aggregate".
#' For the given testcd, visit information from sv dataset would be used.
#' The following Permissible variables have NOT been mapped within the reg function: SPDEVID, EGREFID, EGSPID, EGBEATNO, EGXFN, EGNAM, EGMETHOD, EGLEAD, EGBLFL, EGDRVFL, EGEVAL, EGEVALID, EGCLSIG, EGREPNUM, VISITDY, TAETORD, EGELTM, EGTPTREF, EGRFTDTC.
#' Dependency datasets: dm, se, sv
#'
#' @param testcd Values to be mapped under EGTESTCD variable
#' @param test Values to be mapped under EGTEST variable
#' @param unit_org Values to be mapped under EGORRESU variable (optional) - If no values has been given as input for unit_org, default units will be used for each Tests are as follows: EGHRMN = beats/min, INTP = NA, PRAG =  ms, QRSAG = ms, QTAG = ms, QTCAAG = ms, QTCBAG = ms, QTCFAG = ms, RRAG = ms. List of units allowed as inputs for each Tests are as follows: EGHRMN = beats/min; INTP = NA_character_; PRAG = ms, s; QRSAG = ms, s; QTAG = ms, s; QTCAAG = ms, s; QTCBAG = ms, s; QTCFAG = ms, s; RRAG = ms, s.
#' @param unit_std Values to be mapped under EGSTRESU variable (optional) - If no values has been given as input for unit_std, default units will be used for each Tests are as follows: EGHRMN = beats/min, INTP = NA, PRAG =  ms, QRSAG = ms, QTAG = ms, QTCAAG = ms, QTCBAG = ms, QTCFAG = ms, RRAG = ms. List of units allowed as inputs for each Tests are as follows: EGHRMN = beats/min; INTP = NA_character_; PRAG = ms, s; QRSAG = ms, s; QTAG = ms, s; QTCAAG = ms, s; QTCBAG = ms, s; QTCFAG = ms, s; RRAG = ms, s.
#' @param cat Values to be mapped under EGCAT variable (optional)
#' @param scat Values to be mapped under EGSCAT variable (optional)
#' @param tpt Values to be mapped under EGTPT and EGTPTN variables (optional) - Expected format: TESTCD|TPT|TPTN. Each value must be separated by a pipe (|). TESTCD values should match respective input. For each TPT mentioned under each TESTCD, new duplicated rows will be created within mentioned TESTCD if there are more than one TPT value.
#' @param pos Values to be mapped under EGPOS variable (optional)
#' @param grpid Values to be mapped under EGGRPID variable (optional) - Expected format: TESTCD|VAR1|VAR2|...|VARn. Each value must be separated by a pipe (|). TESTCD values should match respective input. The values under VAR1, VAR2, … VARn would be concatenated separated by a hyphen (-). If same TESTCD value is given as input as more than once, the values for the TESTCDs will be duplicated when each time same TESTCD is added.
#' @param sort_seq Sorting sequence to be used for EGSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "VISITNUM", "EGTESTCD", "EGTEST", "EGDTC", "EGTPTNUM"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be in UPPERCASE
#'
#' @return A data.frame with SDTM EG structure
#' @export
#'
#' @examples
#' \dontrun{
#' eg <- reg(testcd = c("EGHRMN", "INTP", "PRAG", "QRSAG", "QTAG",
#'                      "QTCAAG", "QTCBAG", "QTCFAG", "RRAG"),
#'           test = c("ECG Mean Heart Rate", "Interpretation", "PR Interval, Aggregate",
#'                    "QRS Duration, Aggregate", "QT Interval, Aggregate",
#'                    "QTca Interval, Aggregate", "QTcB Interval, Aggregate",
#'                    "QTcF Interval, Aggregate", "RR Interval, Aggregate"),
#'           unit_org = c("beats/min", NA, "ms", "ms", "ms", "ms", "ms", "ms", "ms"),
#'           unit_std = c("beats/min", NA, "ms", "ms", "ms", "ms", "ms", "ms", "ms"),
#'           cat = c("ECG Results", rep(NA, 7), "ECG Results"),
#'           scat = c(rep(NA, 9)),
#'           tpt = c("EGHRMN|Pre-Dose|-1", "EGHRMN|Post-Dose|1"),
#'           pos = c("SUPINE", rep(NA, 8)),
#'           grpid = c("INTP|USUBJID|VISIT|EGDTC"),
#'           drop_vars = c())
#' }

reg <- function(testcd = c(),
                test = c(),
                unit_org = c(),
                unit_std = c(),
                cat = c(),
                scat = c(),
                tpt = c(),
                pos = c(),
                grpid = c(),
                sort_seq = c("STUDYID", "USUBJID", "VISITNUM", "EGTESTCD", "EGTEST", "EGDTC", "EGTPTNUM"),
                drop_vars = c()) {

  # Metadata for the EG dataset
  eg_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "SPDEVID" = "Sponsor Device Identifier",
                      "EGSEQ" = "Sequence Number",
                      "EGGRPID" = "Group ID",
                      "EGREFID" = "ECG Reference ID",
                      "EGSPID" = "Sponsor-Defined Identifier",
                      "EGBEATNO" = "ECG Beat Number",
                      "EGTESTCD" = "ECG Test or Examination Short Name",
                      "EGTEST" = "ECG Test or Examination Name",
                      "EGCAT" = "Category for ECG",
                      "EGSCAT" = "Subcategory for ECG",
                      "EGPOS" = "ECG Position of Subject",
                      "EGORRES" = "Result or Finding in Original Units",
                      "EGORRESU" = "Original Units",
                      "EGSTRESC" = "Character Result/Finding in Std Format",
                      "EGSTRESN" = "Numeric Result/Finding in Standard Units",
                      "EGSTRESU" = "Standard Units",
                      "EGSTAT" = "Completion Status",
                      "EGREASND" = "Reason ECG Not Done",
                      "EGXFN" = "ECG External File Path",
                      "EGNAM" = "Vendor Name",
                      "EGMETHOD" = "Method of Test or Examination",
                      "EGLEAD" = "Lead Location Used for Measurement",
                      "EGLOBXFL" = "Last Observation Before Exposure Flag",
                      "EGBLFL" = "Baseline Flag",
                      "EGDRVFL" = "Derived Flag",
                      "EGEVAL" = "Evaluator",
                      "EGEVALID" = "Evaluator Identifier",
                      "EGCLSIG" = "Clinically Significant, Collected",
                      "EGREPNUM" = "Repetition Number",
                      "VISITNUM" = "Visit Number",
                      "VISIT" = "Visit Name",
                      "VISITDY" = "Planned Study Day of Visit",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "EGDTC" = "Date/Time of ECG",
                      "EGDY" = "Study Day of ECG",
                      "EGTPT" = "Planned Time Point Name",
                      "EGTPTNUM" = "Planned Time Point Number",
                      "EGELTM" = "Planned Elapsed Time from Time Point Ref",
                      "EGTPTREF" = "Time Point Reference",
                      "EGRFTDTC" = "Date/Time of Reference Time Point")

  # Logic checks
  # Checking the availability of the dependent datasets
  if (!exists("dm") | !exists("se") | !exists("sv")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, se, sv.")
  }

  # Checking if all the required inputs have been provided
  if (missing(test) | missing(testcd)) {
    stop("Error: One or more Req variable input has NOT been provided. Make sure to add inputs for all the following parameters: test, testcd.")
  }

  # No. of TESTCD/TEST provided should be matched
  if (length(testcd) != length(test)) {
    stop("Error: No. of TESTCD and TEST provided not matched")
  }

  # No. of TESTCD/Org Units provided should be matched
  if (length(unit_org) > 0 & length(testcd) != length(unit_org)) {
    stop("Error: No. of Unit_Org and TEST provided not matched")
  }

  # No. of TESTCD/Std Units provided should be matched
  if (length(unit_std) > 0 & length(testcd) != length(unit_std)) {
    stop("Error: No. of Unit_Std and TEST provided not matched")
  }

  # Checking if all the optional parameters has necessary input values or else creating dummy values
  cat <- opt_var_chk(opt_var = "cat", comp_var = "testcd")  # Comparing CAT and TESTCD inputs
  scat <- opt_var_chk(opt_var = "scat", comp_var = "testcd") # Comparing SCAT and TESTCD inputs
  pos <- opt_var_chk(opt_var = "pos", comp_var = "testcd")  # Comparing POS and TESTCD inputs

  # Default unit values
  unit_def <- data.frame(testcd = c("EGHRMN", "INTP", "PRAG", "QRSAG", "QTAG", "QTCAAG", "QTCBAG", "QTCFAG", "RRAG"),
                         unit_org = c("beats/min", NA, "ms", "ms", "ms", "ms", "ms", "ms", "ms" ),
                         unit_std = c("beats/min", NA, "ms", "ms", "ms", "ms", "ms", "ms", "ms" ))

  # TEST/TESTCD dataframe with unit and other optional values for each test
  # If both the unit_org and unit_std are given as inputs
  if (length(unit_org) > 0 & length(unit_std) > 0) {
    test_df <- data.frame(test, testcd, unit_org, unit_std, cat, scat, pos)

    # If both unit_org and unit_std are NOT given as inputs
  } else if (length(unit_org) == 0 & length(unit_std) == 0) {
    test_df <- left_join(data.frame(test, testcd, cat, scat, pos), unit_def %>%  select(testcd, unit_org, unit_std), by = c("testcd"))

    # If unit_org is given but unit_std is NOT given
  } else if (length(unit_org) > 0 & length(unit_std) == 0) {
    test_df <- left_join(data.frame(test, testcd, unit_org, cat, scat, pos), unit_def %>% select(testcd, unit_std), by = c("testcd"))

    # If unit_org is NOT given but unit_std is given
  } else if (length(unit_org) == 0 & length(unit_std) > 0) {
    test_df <- left_join(data.frame(test, testcd, unit_std, cat, scat, pos), unit_def %>% select(testcd, unit_org), by = c("testcd"))
  }

  # Checking if necessary inputs are provided for TPT
  if (!is.null(tpt) & length(tpt) > 0) {
    tpt_in = tpt
    tpt_df <- as.data.frame(tpt_in) %>%
      mutate(testcd = sapply(strsplit(as.character(tpt_in), "\\|"), "[", 1),
             tpt = sapply(strsplit(as.character(tpt_in), "\\|"), "[", 2),
             tptn = as.numeric(sapply(strsplit(as.character(tpt_in), "\\|"), "[", 3)))

    tpt_df1 <- left_join(tpt_df, test_df, by = c("testcd"))

    if (!(all(tpt_df$testcd %in% testcd))){
      stop("Error: Invalid input for TPT. Expected format: TESTCD|TPT|TPTN. Each value must be separated by a pipe (|). Check if the values match the provided TESTCD input.")
    }
    # Creating empty TPT value if no input has been provided
  } else {
    tpt_df1 <- data.frame(testcd = character(), test = character(), tpt = character(), tptn = numeric())
  }

  # Adding TPT rows for TESTCDs as per input
  test_df1 <- test_df %>%
    filter(!(testcd %in% unique(tpt_df1$testcd))) %>%   # Excluding the single row TESTCD which has TPT input
    bind_rows(tpt_df1)

  # Mapping --GRPID variables
  if (length(grpid) > 0) {

    grpid_df <- as.data.frame(grpid) %>%
      mutate(testcd = sapply(strsplit(as.character(grpid), "\\|"), "[", 1),
             var_count = str_count(grpid, "\\|"),
             grpid_vars = mapply(function(test_var, grp_var) gsub(paste0(test_var, "\\|"), "", grp_var), testcd, grpid)
      )

    grpid_df1 <- left_join(grpid_df, test_df, by = c("testcd"))

    # Checking if TESTCD values provided under GRPID mathces the actual TESTCD input
    if (!(all(grpid_df1$testcd %in% testcd))){
      stop("Error: Invalid input for GRPID. Expected format: TESTCD|VAR1|VAR2|...|VARn. Each value must be separated by a pipe (|). Check if the values match the provided TESTCD input.")
    }

    test_df2 <- left_join(test_df1, grpid_df %>% select(testcd, grpid_vars), by = c("testcd"))
  } else {
    test_df2 <- test_df1
  }

  # Allowed units as input based on CT
  unit_eg <- list("EGHRMN" = c("beats/min"),
                  "INTP" = c(NA_character_),
                  "PRAG" = c("ms", "s"),
                  "QRSAG" = c("ms", "s"),
                  "QTAG" = c("ms", "s"),
                  "QTCAAG" = c("ms", "s"),
                  "QTCBAG" = c("ms", "s"),
                  "QTCFAG" = c("ms", "s"),
                  "RRAG" = c("ms", "s")
                  )

  # Function to check if the provided units are as per the allowed unit values
  unit_chk <- function(unit_var) {

    unit_chk_df <- test_df2 %>%
      rowwise() %>%
      mutate(allowed = list(unit_eg[[as.character(testcd)]]),
             invalid_units = ifelse(length(setdiff(!!rlang::sym(unit_var), allowed)) == 0, "",
                                    setdiff(!!rlang::sym(unit_var), allowed))) %>%
      filter(length(invalid_units) > 0 & invalid_units != "") %>%
      select(test, testcd, allowed, unit_org, unit_std, invalid_units)

    if (nrow(unit_chk_df) > 0) {
      pmap(unit_chk_df, function(test, testcd, allowed, unit_org, unit_std, invalid_units) {
        warn_msg <- paste0("Error: ", testcd, " has invalid unit: ", invalid_units, "; Allowed units are: ", paste(allowed, collapse = ", "))
        warning(warn_msg, immediate.=TRUE)
      })
      stop("Error: Invalid units found! Check above messages for details.")
      }
  }

  # Checking unit values against CT values
  unit_chk("unit_org")
  unit_chk("unit_std")

  # Unit conversion values
  conv_factor <- data.frame(unit_org =    c("ms", "s"),
                            unit_std =    c("s", "ms"),
                            conv_factor = c(0.001, 1000))

  # Joining Conversion factor
  test_df3 <- left_join(test_df2, conv_factor, by = c("unit_org", "unit_std"))

  # Input values to be used for generating random results
  res_inputs <- data.frame(testcd = c("EGHRMN", "PRAG", "PRAG", "QRSAG", "QRSAG", "QTAG", "QTAG", "QTCAAG", "QTCAAG", "QTCBAG", "QTCBAG", "QTCFAG", "QTCFAG", "RRAG", "RRAG"),
                           unit_org = c("beats/min", "ms", "s", "ms", "s", "ms", "s", "ms", "s", "ms", "s", "ms", "s", "ms", "s"),
                           mean = c(80, 160, 0.16, 90, 0.09, 400, 0.4, 400, 0.4, 400, 0.4, 400, 0.4, 800, 0.8),
                           sd = c(10, 10, 0.01, 10, 0.01, 10, 0.01, 10, 0.01, 10, 0.01, 10, 0.01, 25, 0.025 ),
                           min = c(20, 90, 0.09, 70, 0.07, 300, 0.3, 300, 0.3, 300, 0.3, 300, 0.3, 400, 0.4),
                           max = c(220, 500, 0.5, 150, 0.15, 500, 0.5, 500, 0.5, 500, 0.5, 500, 0.5, 2500, 2.5),
                           dec = c(0, 0, 3, 0, 3, 0, 3, 0, 3, 0, 3, 0, 3, 0, 3),
                           dec_cf = c(0, 0, 3, 0, 3, 0, 3, 0, 3, 0, 3, 0, 3, 0, 3), # Decimal factor to be used while converting values from org to std
                           na_prob = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

  # Input values to be used for generating random results for INTP test
  intp_res <- c("NORMAL", "ABNORMAL")

  # Joining the mean, sd, min, max, dec, na_prob values to generate random result values
  test_df4 <- left_join(test_df3, res_inputs %>% select(-dec_cf), by = c("testcd", "unit_org")) %>%
              left_join(. , res_inputs %>%  select(testcd, unit_org, dec_cf), by = c("testcd" = "testcd", "unit_std" = "unit_org"))

  # Input SV dataset to create data for each visit occurred
  sv_in <- sv %>%
           filter(SVOCCUR == "Y" | is.na(SVOCCUR) | SVOCCUR == "") %>%
           select(USUBJID, VISIT, VISITNUM, SVSTDTC)

  # Crossing SV with TEST to get test records each subject/visit
  df1 <- crossing(sv_in, test_df4)

  # Creating Result variable
  df1$result <- NA

  for (testcd_loop in testcd) {

    if (testcd_loop != "INTP") {
      temp <- df1[df1$testcd == testcd_loop,]

      df1$result[df1$testcd == testcd_loop] <- rand_res(n = nrow(df1[df1$testcd == testcd_loop,]),
                                                        mean = temp$mean,
                                                        sd = temp$sd,
                                                        min = temp$min,
                                                        max = temp$max,
                                                        dec = temp$dec)
    }
  }

  for (testcd_loop_char in c("INTP")) {

    temp <- df1[df1$testcd == testcd_loop_char,]

    df1$result[df1$testcd == testcd_loop_char] <- rand_res_char(choices = intp_res,
                                                           n = nrow(df1[df1$testcd == testcd_loop_char,]))
  }

  # Mapping general variables and Result variables
  df2 <- df1 %>%
         mutate(STUDYID = get_studyid(),
                DOMAIN = "EG",
                EGTESTCD = testcd,
                EGTEST = test,
                EGCAT = cat,
                EGSCAT = scat,
                EGPOS = pos,
                EGORRES = as.character(result),
                EGORRESU = ifelse(EGORRES != "" & !is.na(EGORRES), as.character(df1$unit_org), NA_character_),
                EGSTRESN = suppressWarnings(case_when(!is.na(conv_factor) & conv_factor != "" ~ round(as.numeric(result) * conv_factor, dec_cf),
                                     TRUE ~ suppressWarnings(as.numeric(result)))),
                EGSTRESU = ifelse(EGSTRESN != "" & !is.na(EGSTRESN), as.character(df1$unit_std), NA_character_),
                EGSTAT = if_else(is.na(EGORRES) | EGORRES == "", "NOT DONE", "", missing = "NOT DONE"),
                EGREASND = if_else(is.na(EGORRES) | EGORRES == "", "UNKNOWN", "", missing = "UNKNOWN"),
                EGDTC = SVSTDTC,
                EGTPT = tpt,
                EGTPTNUM = tptn) %>%
         rowwise() %>%
         mutate(EGSTRESC = coalescec(as.character(EGSTRESN),EGORRES)) %>%
         ungroup()

  # --GRPID mapping
  if (length(grpid) > 0 ) {

    # Checking if input variables are available
    vars <- strsplit(grpid, "\\|")

    for (rows in seq_along(vars)){
      vars_row <- vars[[rows]]

      for (var in seq_along(vars_row)){

        if (var > 1) {   #Ignoring the TESTCD value

          if (!(vars_row[var]  %in% names(df2))) {
            stop("Error: Variable used to group is not an valid variable: ", vars_row[var])
          }
        }
      }
    }

    # Mapping --GRPID Variable separated by hyphen (-)
    df3 <- df2 %>%
           mutate(EGGRPID = pmap_chr(., function(..., grpid_vars) {   # pmap_chr() applies a function to each row
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
    df3 <- df2 %>%
           mutate(EGGRPID = NA_character_)
  }

  # Mapping LOBXFL variable
  df4 <- lobxfl(df = df3, dtc = "EGDTC", res_var = "EGORRES", sort = c("USUBJID", "EGTESTCD", "EGTPT"))

  # Mapping EPOCH variable
  df5 <- epoch(df = df4, dtc = "EGDTC")

  # Mapping --DY variable
  df6 <- stdy(df = df5, dtc = "EGDTC")

  # Mapping SEQ variable
  df7 <- seqnum(df = df6, sort = sort_seq)

  # Keeping only the Necessary variables
  df8 <- df7 %>%
         select(STUDYID, DOMAIN, USUBJID, EGSEQ, EGGRPID, EGTESTCD, EGTEST, EGCAT, EGSCAT, EGPOS, EGORRES, EGORRESU,
                EGSTRESC, EGSTRESN, EGSTRESU, EGSTAT, EGREASND, EGLOBXFL, VISITNUM, VISIT, EPOCH, EGDTC,
                EGDY, EGTPT, EGTPTNUM)

  # Adding labels to the variables
  eg <- apply_metadata(df8, eg_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    eg <- eg %>% select(-all_of(drop_vars))
  }

  # Final EG dataset
  return(eg)
}
