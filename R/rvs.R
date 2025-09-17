# R/rvs.R
#' Create Random Vital Signs (VS) Dataset
#'
#' @description
#' Creates a random SDTM VS (Vital Signs) dataset following CDISC SDTM standards.
#' Findings. One record per vital sign measurement per time point per visit per subject, Tabulation.
#' Function is designed to create values for only the following Tests: Weight, Height, Body Mass Index, Temperature, Heart Rate, Pulse Rate, Respiratory Rate, Diastolic Blood Pressure, Systolic Blood Pressure.
#' The following Permissible variables have NOT been mapped within the rvs function: VSSPID, VSBLFL, VSDRVFL, VSTOX, VSTOXGR, VSCLSIG, VISITDY, TAETORD, VSELTM, VSTPTREF, VSRFTDTC.
#' Dependency datasets: dm, se, sv
#'
#' @param domain By default, value has been set as "VS", user can modify it if needed but not recommended
#' @param testcd Values to be mapped under VSTESTCD variable
#' @param test Values to be mapped under VSTEST variable
#' @param unit_org Values to be mapped under VSORRESU variable (optional) - If no values has been given as input for unit_org, default units will be used for each Tests are as follows: WEIGHT = kg, HEIGHT = cm, BMI = kg/m2, TEMP = C,  HR = beats/min, PULSE =  beats/min, RESP = breaths/min, DIABP = mmHg, SYSBP = mmHg. List of units allowed as inputs for each Tests are as follows: WEIGHT = g, kg; HEIGHT = cm, ft, in, m, mm; BMI = kg/m2; TEMP = C, F, K; HR = beats/min; PULSE = beats/min; RESP = breaths/min; DIABP = cmHg, mmHg; SYSBP = cmHg, mmHg.
#' @param unit_std Values to be mapped under VSSTRESU variable (optional) - If no values has been given as input for unit_std, default units will be used for each Tests are as follows: WEIGHT = kg, HEIGHT = cm, BMI = kg/m2, TEMP = C,  HR = beats/min, PULSE =  beats/min, RESP = breaths/min, DIABP = mmHg, SYSBP = mmHg. List of units allowed as inputs for each Tests are as follows: WEIGHT = g, kg; HEIGHT = cm, ft, in, m, mm; BMI = kg/m2; TEMP = C, F, K; HR = beats/min; PULSE = beats/min; RESP = breaths/min; DIABP = cmHg, mmHg; SYSBP = cmHg, mmHg.
#' @param cat Values to be mapped under VSCAT variable (optional)
#' @param scat Values to be mapped under VSSCAT variable (optional)
#' @param tpt Values to be mapped under VSTPT and VSTPTN variables (optional) - Expected format: TESTCD|TPT|TPTN. Each value must be separated by a pipe (|). TESTCD values should match respective input. For each TPT mentioned under each TESTCD, new duplicated rows will be created within mentioned TESTCD if there are more than one TPT value.
#' @param pos Values to be mapped under VSPOS variable (optional)
#' @param loc Values to be mapped under VSLOC variable (optional)
#' @param lat Values to be mapped under VSLAT variable (optional)
#' @param grpid Values to be mapped under VSGRPID variable (optional) - Expected format: TESTCD|VAR1|VAR2|...|VARn. Each value must be separated by a pipe (|). TESTCD values should match respective input. The values under VAR1, VAR2, … VARn would be concatenated separated by a hyphen (-). If same TESTCD value is given as input as more than once, the values for the TESTCDs will be duplicated when each time same TESTCD is added.
#' @param sort_seq Sorting sequence to be used for VSSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "VISITNUM", "VSTESTCD", "VSTEST", "VSDTC", "VSTPTNUM"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be listed in UPPERCASE
#'
#' @return A data.frame with SDTM VS structure
#' @export
#'
#' @examples
#' rvs(testcd = c("WEIGHT", "HEIGHT", "BMI", "TEMP", "HR", "PULSE", "RESP", "DIABP", "SYSBP"),
#'     test = c("Weight", "Height", "Body Mass Index", "Temperature", "Heart Rate", "Pulse Rate", "Respiratory Rate", "Diastolic Blood Pressure", "Systolic Blood Pressure"),
#'     unit_org = c("g", "mm", "kg/m2", "F", "beats/min", "beats/min", "breaths/min", "cmHg", "mmHg"),
#'     unit_std = c("kg", "cm", "kg/m2", "C", "beats/min", "beats/min", "breaths/min", "mmHg", "cmHg"),
#'     cat = c("VITAL SIGNS", "VITAL SIGNS", rep(NA, 5), "VITAL SIGNS", "VITAL SIGNS"),
#'     scat = c(rep(NA, 7), "Blood Pressure", "Blood Pressure"),
#'     tpt = c("HR|Pre-Dose|-1", "HR|Post-Dose|1"),
#'     pos = c("STANDING", rep(NA, 8)),
#'     loc = c("ARM", rep(NA, 8)),
#'     lat = c("LEFT", rep(NA, 8)),
#'     grpid = c("WEIGHT|USUBJID|VISIT|VSDTC", "HEIGHT|USUBJID|VISIT|VSDTC|VSTEST", "HR|USUBJID|VISIT|VSDTC|VSTEST"),
#'     drop_vars = c())
#'
rvs <- function(domain = "VS",
                testcd = c(),
                test = c(),
                unit_org = c(),
                unit_std = c(),
                cat = c(),
                scat = c(),
                tpt = c(),
                pos = c(),
                loc = c(),
                lat = c(),
                grpid = c(),
                sort_seq = c("STUDYID", "USUBJID", "VISITNUM", "VSTESTCD", "VSTEST", "VSDTC", "VSTPTNUM"),
                drop_vars = c()) {

  # Metadata for the VS dataset
  vs_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "VSSEQ" = "Sequence Number",
                      "VSGRPID" = "Group ID",
                      "VSSPID" = "Sponsor-Defined Identifier",
                      "VSTESTCD" = "Vital Signs Test Short Name",
                      "VSTEST" = "Vital Signs Test Name",
                      "VSCAT" = "Category for Vital Signs",
                      "VSSCAT" = "Subcategory for Vital Signs",
                      "VSPOS" = "Vital Signs Position of Subject",
                      "VSORRES" = "Result or Finding in Original Units",
                      "VSORRESU" = "Original Units",
                      "VSSTRESC" = "Character Result/Finding in Std Format",
                      "VSSTRESN" = "Numeric Result/Finding in Standard Units",
                      "VSSTRESU" = "Standard Units",
                      "VSSTAT" = "Completion Status",
                      "VSREASND" = "Reason Not Performed",
                      "VSLOC" = "Location of Vital Signs Measurement",
                      "VSLAT" = "Laterality",
                      "VSLOBXFL" = "Last Observation Before Exposure Flag",
                      "VSBLFL" = "Baseline Flag",
                      "VSDRVFL" = "Derived Flag",
                      "VSTOX" = "Toxicity",
                      "VSTOXGR" = "Standard Toxicity Grade",
                      "VSCLSIG" = "Clinically Significant, Collected",
                      "VISITNUM" = "Visit Number",
                      "VISIT" = "Visit Name",
                      "VISITDY" = "Planned Study Day of Visit",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "VSDTC" = "Date/Time of Measurements",
                      "VSDY" = "Study Day of Vital Signs",
                      "VSTPT" = "Planned Time Point Name",
                      "VSTPTNUM" = "Planned Time Point Number",
                      "VSELTM" = "Planned Elapsed Time from Time Point Ref",
                      "VSTPTREF" = "Time Point Reference",
                      "VSRFTDTC" = "Date/Time of Reference Time Point"
                      )

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
  loc <- opt_var_chk(opt_var = "loc", comp_var = "testcd")  # Comparing LOC and TESTCD inputs
  lat <- opt_var_chk(opt_var = "lat", comp_var = "testcd")  # Comparing LAT and TESTCD inputs

  # Default unit values
  unit_def <- data.frame(testcd = c("WEIGHT", "HEIGHT", "BMI", "TEMP", "HR", "PULSE", "RESP", "DIABP", "SYSBP"),
                         unit_org = c("kg", "cm", "kg/m2", "C", "beats/min", "beats/min", "breaths/min", "mmHg", "mmHg"),
                         unit_std = c("kg", "cm", "kg/m2", "C", "beats/min", "beats/min", "breaths/min", "mmHg", "mmHg"))


  # TEST/TESTCD dataframe with unit and other optional values for each test
  # If both the unit_org and unit_std are given as inputs
  if (length(unit_org) > 0 & length(unit_std) > 0) {
    test_df <- data.frame(test, testcd, unit_org, unit_std, cat, scat, pos, lat, loc)

  # If both unit_org and unit_std are NOT given as inputs
  } else if (length(unit_org) == 0 & length(unit_std) == 0) {
    test_df <- left_join(data.frame(test, testcd, cat, scat, pos, lat, loc), unit_def %>%  select(testcd, unit_org, unit_std), by = c("testcd"))

    # If unit_org is given but unit_std is NOT given
  } else if (length(unit_org) > 0 & length(unit_std) == 0) {
    test_df <- left_join(data.frame(test, testcd, unit_org, cat, scat, pos, lat, loc), unit_def %>% select(testcd, unit_std), by = c("testcd"))

    # If unit_org is NOT given but unit_std is given
  } else if (length(unit_org) == 0 & length(unit_std) > 0) {
    test_df <- left_join(data.frame(test, testcd, unit_std, cat, scat, pos, lat, loc), unit_def %>% select(testcd, unit_org), by = c("testcd"))
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
  unit_vs <- list("WEIGHT" = c("g", "kg"),
                  "HEIGHT" = c("cm", "ft", "in", "m", "mm"),
                  "BMI" = c("kg/m2"),
                  "TEMP" = c("C", "F", "K"),
                  "HR" = c("beats/min"),
                  "PULSE" = c("beats/min"),
                  "RESP" = c("breaths/min"),
                  "DIABP" = c("cmHg", "mmHg"),
                  "SYSBP" = c("cmHg", "mmHg")
                  )

  # Function to check if the provided units are as per the allowed unit values
  unit_chk <- function(unit_var) {

    unit_chk_df <- test_df2 %>%
                   rowwise() %>%
                   mutate(allowed = list(unit_vs[[as.character(testcd)]]),
                          invalid_units = ifelse(length(setdiff(!!rlang::sym(unit_var), allowed)) == 0, "",
                                                 setdiff(!!rlang::sym(unit_var), allowed))) %>%
                   filter(length(invalid_units) > 0 & invalid_units != "") %>%
                   select(test, testcd, allowed, unit_org, unit_std, invalid_units)

    if (nrow(unit_chk_df) > 0 ) {
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
  # Weight - Conversion factors
  weight_cf <- data.frame(unit_org =    c("g", "kg"),
                          unit_std =    c("kg", "g"),
                          conv_factor = c(0.001, 1000))

  # Height - Conversion factors
  height_cf <- data.frame(unit_org =    c("cm", "cm", "cm", "cm", "ft", "ft", "ft", "ft",
                                          "in", "in", "in", "in", "m", "m", "m", "m",
                                          "mm", "mm", "mm", "mm"),
                          unit_std =    c("ft", "in", "m", "mm", "cm", "in", "m", "mm",
                                          "cm", "ft", "m", "mm", "cm", "ft", "in", "mm",
                                          "cm", "ft", "in", "m"),
                          conv_factor = c(0.0328084, 0.393701, 0.01, 10, 30.48, 12, 0.3048, 304.8,
                                          2.54, 0.0833333, 0.0254, 25.4, 100, 3.28084, 39.3701, 1000,
                                          0.1, 0.00328084, 0.0393701, 0.001))

  # Sys/Dia BP - Conversion factors
  bp_cf <- data.frame(unit_org =    c("cmHg", "mmHg"),
                      unit_std =    c("mmHg", "cmHg"),
                      conv_factor = c(10, 0.1))

  conv_factor <- bind_rows(weight_cf, height_cf, bp_cf)

  # Joining Conversion factor
  test_df3 <- left_join(test_df2, conv_factor, by = c("unit_org", "unit_std"))

  # Input values to be used for generating random results
  res_inputs <- data.frame(testcd = c("WEIGHT", "WEIGHT", "HEIGHT", "HEIGHT", "HEIGHT", "HEIGHT", "HEIGHT", "BMI", "TEMP", "TEMP", "TEMP", "HR", "PULSE", "RESP", "DIABP", "DIABP", "SYSBP", "SYSBP"),
                           unit_org = c("g", "kg", "cm", "ft", "in", "m", "mm", "kg/m2", "C", "F", "K", "beats/min", "beats/min", "breaths/min", "mmHg", "cmHg", "mmHg", "cmHg"),
                           mean = c(70000, 70, 170, 5.5, 70, 1.7, 1700, 25, 37, 98.5, 310, 80, 80, 18, 80, 8, 120, 12),
                           sd = c(8000, 8, 15, 0.5, 5, 0.2, 150, 4, 2, 3, 2, 10, 10, 3, 5, 0.5, 5, 0.5),
                           min = c(0, 0, 0, 0, 0, 0, 0, 0, 15, 59, 285, 20, 20, 5, 30, 3, 50, 5) ,
                           max = c(200000, 200, 250, 8, 100, 2.5, 2500, 60, 55, 131, 330, 220, 220, 45, 130, 13, 190, 19),
                           dec = c(1, 1, 0, 2, 2, 2, 2, 1, 1, 1, 2, 0, 0, 0, 0, 1, 0, 1),
                           dec_cf = c(1, 1, 0, 2, 2, 2, 2, 1, 1, 1, 2, 0, 0, 0, 0, 1, 0, 1), # Decimal factor to be used while converting values from org to std
                           na_prob = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

  # Joining the mean, sd, min, max, dec, na_prob values to generate random result values
  test_df4 <- left_join(test_df3, res_inputs %>% select(-dec_cf), by = c("testcd", "unit_org")) %>%
              left_join(. , res_inputs %>%  select(testcd, unit_org, dec_cf), by = c("testcd" = "testcd", "unit_std" = "unit_org"))

  # Input SV dataset to create data for each visit occurred
  sv_in <- sv %>%
           filter(SVOCCUR == "Y") %>%
           select(USUBJID, VISIT, VISITNUM, SVSTDTC)

  # Crossing SV with TEST to get test records each subject/visit
  df1 <- crossing(sv_in, test_df4)

  # Creating Result variable
  df1$result <- NA

  for (testcd_loop in testcd) {

    temp <- df1[df1$testcd == testcd_loop,]

    df1$result[df1$testcd == testcd_loop] <- rand_res(n = nrow(df1[df1$testcd == testcd_loop,]),
                                                      mean = temp$mean,
                                                      sd = temp$sd,
                                                      min = temp$min,
                                                      max = temp$max,
                                                      dec = temp$dec)
  }

  df2 <- df1 %>%
         mutate(result_temp_conv = case_when(testcd == "TEMP" & unit_org == "C" & unit_std == "C" ~ round(result,1),
                                             testcd == "TEMP" & unit_org == "C" & unit_std == "F" ~ round((result * (9/5)) + 32, 1),
                                             testcd == "TEMP" & unit_org == "C" & unit_std == "K" ~ round(result + 273.15, 2),
                                             testcd == "TEMP" & unit_org == "F" & unit_std == "F" ~ round(result, 1),
                                             testcd == "TEMP" & unit_org == "F" & unit_std == "C" ~ round((result - 32) * (5/9), 1),
                                             testcd == "TEMP" & unit_org == "F" & unit_std == "K" ~ round(((result - 32) * (5/9)) + 273.15, 2),
                                             testcd == "TEMP" & unit_org == "K" & unit_std == "K" ~ round(result, 2),
                                             testcd == "TEMP" & unit_org == "K" & unit_std == "C" ~ round(result - 273.15, 1),
                                             testcd == "TEMP" & unit_org == "K" & unit_std == "F" ~ round(((result - 273.15) * (9/5)) + 32, 1),
                                             TRUE ~ NA_real_
                                             ))

  # Mapping general variables and Result variables
  df3 <- df2 %>%
         mutate(STUDYID = studyid,
                DOMAIN = domain,
                VSTESTCD = testcd,
                VSTEST = test,
                VSCAT = cat,
                VSSCAT = scat,
                VSPOS = pos,
                VSORRES = as.character(result),
                VSORRESU = ifelse(VSORRES != "" & !is.na(VSORRES), as.character(df2$unit_org), NA_character_),
                VSSTRESN = case_when(!is.na(conv_factor) & conv_factor != "" ~ round(result * conv_factor, dec_cf),
                                     testcd == "TEMP" ~ result_temp_conv,
                                     TRUE ~ result),
                VSSTRESC = as.character(VSSTRESN),
                VSSTRESU = ifelse(VSSTRESC != "" & !is.na(VSSTRESC), as.character(df2$unit_std), NA_character_),
                VSSTAT = if_else(is.na(VSORRES) | VSORRES == "", "NOT DONE", "", missing = "NOT DONE"),
                VSREASND = if_else(is.na(VSORRES) | VSORRES == "", "UNKNOWN", "", missing = "UNKNOWN"),
                VSLOC = loc,
                VSLAT = lat,
                VSDTC = SVSTDTC,
                VSTPT = tpt,
                VSTPTNUM = tptn)


  # --GRPID mapping
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
           mutate(VSGRPID = pmap_chr(., function(..., grpid_vars) {   # pmap_chr() applies a function to each row
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
           mutate(VSGRPID = NA_character_)
  }

  # Mapping LOBXFL variable
  df5 <- lobxfl(df = df4, dtc = "VSDTC", res_var = "VSORRES", sort = c("USUBJID", "VSTESTCD", "VSTPT"))

  # Mapping EPOCH variable
  df6 <- epoch(df = df5, dtc = "VSDTC")

  # Mapping --DY variable
  df7 <- stdy(df = df6, dtc = "VSDTC")

  # Mapping SEQ variable
  df8 <- seqnum(df = df7, sort = sort_seq)

  # Keeping only the Necessary variables
  df9 <- df8 %>%
         select(STUDYID, DOMAIN, USUBJID, VSSEQ, VSGRPID, VSTESTCD, VSTEST, VSCAT, VSSCAT, VSPOS, VSORRES, VSORRESU,
                VSSTRESC, VSSTRESN, VSSTRESU, VSSTAT, VSREASND, VSLOC, VSLAT, VSLOBXFL, VISITNUM, VISIT, EPOCH, VSDTC,
                VSDY, VSTPT, VSTPTNUM)

  # Adding labels to the variables
  df10 <- apply_metadata(df9, vs_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    df10 <- df10 %>% select(-all_of(drop_vars))
  }

  # Final VS dataset
  assign("vs", df10, envir = .GlobalEnv)

}
