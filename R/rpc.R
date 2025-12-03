# R/rpc.R
#' Create Random Pharmacokinetics Concentrations (PC) Dataset
#'
#' @description
#' Creates a random SDTM PC (Pharmacokinetics Concentrations) dataset following CDISC SDTM standards.
#' Findings. One record per sample characteristic or time-point concentration per reference time point or per analyte per subject, Tabulation.
#' EXTRT from EX dataset would be used as PCTEST. Respective ARMCD values would be used as PCTESTCD by restricting its value to 8 characters.
#' The following Permissible variables have NOT been mapped within the rpc function: PCREFID, PCSPID, PCSPCCND, PCMETHOD, PCFAST, PCDRVFL, PCULOQ, VISITDY, TAETORD, PCENDTC, PCENDY, PCEVLINT.
#' Dependency datasets: dm, ex, tv, ta, se
#'
#' @param domain By default, value has been set as "PC", user can modify it if needed but not recommended
#' @param cat Values to be mapped under PCCAT variable - Expected format: TRT|PCCAT value. Each value must be separated by a pipe (|). TRT values should match with EXTRT values in EX dataset.
#' @param scat Values to be mapped under PCSCAT variable - Expected format: TRT|PCSCAT value. Each value must be separated by a pipe (|). TRT values should match with EXTRT values in EX dataset.
#' @param tpt Timepoint values for each TRT. Input values should be given along with TRT value. Expected format: TRT|Timepoint values separated by comma. Example: "Adalimumab|Pre-Dose, 1 hr Post-Dose, 2 hr Post-Dose, 6 hr Post-Dose, 12 hr Post-Dose, 24 hr Post-Dose" - If "Adalimumab" EXTRT has six Timepoint values. If no inputs are given, Timepoint values would be "Pre-Dose, 15 min Post-Dose, 30 min Post-Dose, 1 hr Post-Dose, 2 hr Post-Dose, 4 hr Post-Dose, 6 hr Post-Dose, 12 hr Post-Dose, 24 hr Post-Dose" by default.
#' @param tptn Timepoint numeric values for each TRT. Input values should be given along with TRT value. Expected format: TRT|Timepoint numeric values separated by comma. Example: "Adalimumab|-1, 1, 2, 3, 4, 5" - If "Adalimumab" EXTRT has six Timepoint numeric values. If no inputs are given, Timepoint numeric values would be "-1, 1, 2, 3, 4, 5" by default.
#' @param unit_org Values to be mapped under PCORRESU variable - Expected format: TRT|PCORRESU value. Each value must be separated by a pipe (|). TRT values should match with EXTRT values in EX dataset. Allowed units are "ng/mL", "ug/mL". If no inputs are given, Unit value would be "ng/mL" by default.
#' @param unit_std Values to be mapped under PCSTRESU variable - Expected format: TRT|PCSTRESU value. Each value must be separated by a pipe (|). TRT values should match with EXTRT values in EX dataset. Allowed units are "ng/mL", "ug/mL". If no inputs are given, Unit value would be "ng/mL" by default.
#' @param visn Values to be mapped under VISITNUM variable - Expected format: TRT|VISITNUM values separated by comma. Each value must be separated by a pipe (|). Example: "Adalimumab|1, 2" - If "Adalimumab" EXTRT has two VISITNUM values. If no inputs are given, VISITNUM value would be the minimum value of VISITNUM from TV dataset by excluding Screening/Baseline visits by default.
#' @param nam Values to be mapped under PCNAM variable - Expected format: TRT|PCNAM value. Each value must be separated by a pipe (|). TRT values should match with EXTRT values in EX dataset.
#' @param spec Values to be mapped under PCSPEC variable - Expected format: TRT|PCSPEC value. Each value must be separated by a pipe (|). TRT values should match with EXTRT values in EX dataset. Default value is "PLASMA" for all records.
#' @param sort_seq Sorting sequence to be used for PCSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "PCTESTCD", "PCTEST", "VISITNUM", "PCDTC", "PCTPTNUM"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped - Variabe names should be in UPPERCASE
#'
#' @return A data.frame with SDTM PC structure
#' @export
#'
#' @examples
#' rpc(cat = c("Adalimumab|ANALYTE"),
#'     tpt = c("Dermavalimab drug|Pre-Dose, Post 5 min, Post 15 min, Post 30 min, Post 1 hr, Post 2hr, Post 3 hr, Post 4 hr, Post 5 hr, Post 6 hr, Post 12 hr"),
#'     tptn = c("Dermavalimab drug|-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10"))
#'
#' rpc()
#'

rpc <- function(domain = "PC",
                cat = c(),
                scat = c(),
                tpt = c(),
                tptn = c(),
                unit_org = c(),
                unit_std = c(),
                visn = c(),
                nam = c(),
                spec = c(),
                sort_seq = c("STUDYID", "USUBJID", "PCTESTCD", "PCTEST", "VISITNUM", "PCDTC", "PCTPTNUM"),
                drop_vars = c()) {

  # Metadata for the PC dataset
  pc_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "PCSEQ" = "Sequence Number",
                      "PCGRPID" = "Group ID",
                      "PCREFID" = "Reference ID",
                      "PCSPID" = "Sponsor-Defined Identifier",
                      "PCTESTCD" = "Pharmacokinetic Test Short Name",
                      "PCTEST" = "Pharmacokinetic Test Name",
                      "PCCAT" = "Test Category",
                      "PCSCAT" = "Test Subcategory",
                      "PCORRES" = "Result or Finding in Original Units",
                      "PCORRESU" = "Original Units",
                      "PCSTRESC" = "Character Result/Finding in Std Format",
                      "PCSTRESN" = "Numeric Result/Finding in Standard Units",
                      "PCSTRESU" = "Standard Units",
                      "PCSTAT" = "Completion Status",
                      "PCREASND" = "Reason Test Not Done",
                      "PCNAM" = "Vendor Name",
                      "PCSPEC" = "Specimen Material Type",
                      "PCSPCCND" = "Specimen Condition",
                      "PCMETHOD" = "Method of Test or Examination",
                      "PCFAST" = "Fasting Status",
                      "PCDRVFL" = "Derived Flag",
                      "PCLLOQ" = "Lower Limit of Quantitation",
                      "PCULOQ" = "Upper Limit of Quantitation",
                      "VISITNUM" = "Visit Number",
                      "VISIT" = "Visit Name",
                      "VISITDY" = "Planned Study Day of Visit",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "PCDTC" = "Date/Time of Specimen Collection",
                      "PCENDTC" = "End Date/Time of Specimen Collection",
                      "PCDY" = "Actual Study Day of Specimen Collection",
                      "PCENDY" = "Study Day of End of Observation",
                      "PCTPT" = "Planned Time Point Name",
                      "PCTPTNUM" = "Planned Time Point Number",
                      "PCELTM" = "Planned Elapsed Time from Time Point Ref",
                      "PCTPTREF" = "Time Point Reference",
                      "PCRFTDTC" = "Date/Time of Reference Point",
                      "PCEVLINT" = "Evaluation Interval")

  # Logic checks
  # Checking the availability of the dependent datasets
  if (!exists("dm") | !exists("ex") | !exists("se") | !exists("tv") | !exists("ta")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, ex, se, tv, ta.")
  }

  # Checking if necessary inputs are provided for cat
  if (!is.null(cat) & length(cat) > 0) {
    cat_in = cat
    cat_df <- as.data.frame(cat_in) %>%
              mutate(TRT = sapply(strsplit(as.character(cat_in), "\\|"), "[", 1),
                     cat = sapply(strsplit(as.character(cat_in), "\\|"), "[", 2))
  } else {
    cat_df <- data.frame(TRT = character(0), cat = character(0))
  }

  # Checking if necessary inputs are provided for scat
  if (!is.null(scat) & length(scat) > 0) {
    scat_in = scat
    scat_df <- as.data.frame(scat_in) %>%
      mutate(TRT = sapply(strsplit(as.character(scat_in), "\\|"), "[", 1),
             scat = sapply(strsplit(as.character(scat_in), "\\|"), "[", 2))
  } else {
    scat_df <- data.frame(TRT = character(0), scat = character(0))
  }

  # Checking if necessary inputs are provided for visn
  if (!is.null(visn) & length(visn) > 0) {
    visn_in = visn
    visn_df <- as.data.frame(visn_in) %>%
               mutate(TRT = sapply(strsplit(as.character(visn_in), "\\|"), "[", 1),
                      visn = sapply(strsplit(as.character(visn_in), "\\|"), "[", 2))
  } else {
    visn_df <- data.frame(TRT = character(0), visn = numeric(0))
  }

  # Checking if necessary inputs are provided for tpt
  if (!is.null(tpt) & length(tpt) > 0) {
    tpt_in = tpt
    tpt_df <- as.data.frame(tpt_in) %>%
              mutate(TRT = sapply(strsplit(as.character(tpt_in), "\\|"), "[", 1),
                     tpt = sapply(strsplit(as.character(tpt_in), "\\|"), "[", 2))
  } else {
    tpt_df <- data.frame(TRT = character(0), tpt = character(0))
  }

  # Checking if necessary inputs are provided for tptn
  if (!is.null(tptn) & length(tptn) > 0) {
    tptn_in = tptn
    tptn_df <- as.data.frame(tptn_in) %>%
               mutate(TRT = sapply(strsplit(as.character(tptn_in), "\\|"), "[", 1),
                      tptn = sapply(strsplit(as.character(tptn_in), "\\|"), "[", 2))
  } else {
    tptn_df <- data.frame(TRT = character(0), tptn = character(0))
  }

  # Checking if necessary inputs are provided for spec
  if (!is.null(spec) & length(spec) > 0) {
    spec_in = spec
    spec_df <- as.data.frame(spec_in) %>%
               mutate(TRT = sapply(strsplit(as.character(spec_in), "\\|"), "[", 1),
                      spec = sapply(strsplit(as.character(spec_in), "\\|"), "[", 2))
  } else {
    spec_df <- data.frame(TRT = character(0), spec = character(0))
  }

  # Checking if necessary inputs are provided for nam
  if (!is.null(nam) & length(nam) > 0) {
    nam_in = nam
    nam_df <- as.data.frame(nam_in) %>%
              mutate(TRT = sapply(strsplit(as.character(nam_in), "\\|"), "[", 1),
                     nam = sapply(strsplit(as.character(nam_in), "\\|"), "[", 2))
  } else {
    nam_df <- data.frame(TRT = character(0), nam = character(0))
  }

  # Logic checks
  chk1 <- tpt_df %>% separate_rows(tpt, sep = "\\s*,\\s*") %>% group_by(TRT) %>% summarise(tpt_obs = n())
  chk2 <- tptn_df %>% separate_rows(tptn, sep = "\\s*,\\s*") %>% group_by(TRT) %>% summarise(tptn_obs = n())

  comb_chk <- full_join(chk1, chk2, by = ("TRT" = "TRT")) %>%
              mutate(tpt_obs = if_else(is.na(as.numeric(tpt_obs)), 0, as.numeric(tpt_obs), missing = 0 ),
                     tptn_obs = if_else(is.na(as.numeric(tptn_obs)), 0, as.numeric(tptn_obs), missing = 0 ))

  # Checking if TRT values provided are matching with actual EXTRT
  for (trt_in in comb_chk$TRT) {

    temp_df <- comb_chk %>% filter(TRT == trt_in)

    if (!(all(temp_df$TRT %in% unique(ex$EXTRT)))) {
      stop(paste0("Error: In tpt/tptn input, provided TRT value ", trt_in," is not matching with EXTRT values. Make sure to use values available in EXTRT from EX as input."))
    }
  }

  # Checking if no. of tpt and tptn provided are matching within each trt
  for (trt_in in comb_chk$TRT) {

    temp_df <- comb_chk %>% filter(TRT == trt_in)

    if (temp_df$tpt_obs != temp_df$tptn_obs) {
      stop(paste0("Error: No. of TPT and No. of TPTN provided are not matching for the following TRT: ", trt_in))
    }
  }

  # Checking if necessary inputs are provided for unit_org
  if (!is.null(unit_org) & length(unit_org) > 0) {
    unit_org_in = unit_org
    unit_org_df <- as.data.frame(unit_org_in) %>%
                   mutate(TRT = sapply(strsplit(as.character(unit_org_in), "\\|"), "[", 1),
                          unit_org = sapply(strsplit(as.character(unit_org_in), "\\|"), "[", 2))
  } else {
    unit_org_df <- data.frame(TRT = character(0), unit_org = character(0))
  }

  # Checking if necessary inputs are provided for unit_std
  if (!is.null(unit_std) & length(unit_std) > 0) {
    unit_std_in = unit_std
    unit_std_df <- as.data.frame(unit_std_in) %>%
                   mutate(TRT = sapply(strsplit(as.character(unit_std_in), "\\|"), "[", 1),
                          unit_std = sapply(strsplit(as.character(unit_std_in), "\\|"), "[", 2))
  } else {
    unit_std_df <- data.frame(TRT = character(0), unit_std = character(0))
  }

  # Allowed units as input based on CT
  unit_pc <- list("ng/mL", "ug/mL")

  # Checking if the provided org units are as per the allowed unit values
  if (!all(unit_org_df$unit_org %in% unit_pc)) {
    stop("Error: Invalid unit found for unit_org. Allowed units are: ng/mL, ug/mL")
  }

  # Checking if the provided std units are as per the allowed unit values
  if (!all(unit_std_df$unit_std %in% unit_pc)) {
    stop("Error: Invalid unit found for unit_std. Allowed units are: ng/mL, ug/mL")
  }

  # Checking if TRT values provided as input for unit_org are matching with actual EXTRT
  for (trt_in in unit_org_df$TRT) {

    temp <- unit_org_df %>% filter(TRT == trt_in)

    if (!(all(temp$TRT %in% unique(ex$EXTRT)))) {
      stop(paste0("Error: In unit_org input, provided TRT value ", trt_in," is not matching with EXTRT values. Make sure to use values available in EXTRT from EX as input."))
    }
  }

  # Checking if TRT values provided as input for unit_std are matching with actual EXTRT
  for (trt_in in unit_std_df$TRT) {

    temp <- unit_std_df %>% filter(TRT == trt_in)

    if (!(all(temp$TRT %in% unique(ex$EXTRT)))) {
      stop(paste0("Error: In unit_std input, provided TRT value ", trt_in," is not matching with EXTRT values. Make sure to use values available in EXTRT from EX as input."))
    }
  }

  # Conversion factors
  pc_cf <- data.frame(unit_org =    c("ng/mL", "ug/mL"),
                      unit_std =    c("ug/mL", "ng/mL"),
                      conv_factor = c(0.001, 1000))

  # Default tpt, tptn, n_tpt values
  n_tpt_default = 9
  tpt_default = c("Pre-Dose, 15 min Post-Dose, 30 min Post-Dose, 1 hr Post-Dose, 2 hr Post-Dose, 4 hr Post-Dose, 6 hr Post-Dose, 12 hr Post-Dose, 24 hr Post-Dose")
  tptn_default = c("-1, 1, 2, 3, 4, 5, 6, 7, 8")

  # Default visn values
  visn_temp <- tv %>%
               filter(!grepl("SCREENING", toupper(VISIT)) & !grepl("BASELINE", toupper(VISIT))) %>%
               filter(min(VISITNUM) == VISITNUM) %>%
               select(VISITNUM)
  visn_default <- visn_temp$VISITNUM

  # Default spec value
  spec_default <- "PLASMA"

  # TESTCD values derived from ARMCD
  testcd <- ta %>%
            select(ARMCD, ARM) %>%
            distinct() %>%
            arrange(ARM, ARMCD) %>%
            mutate(ARMCD_8 = substr(gsub("[[:space:]]+", "", ARMCD), 1, 8)) %>%
            group_by(ARMCD_8) %>%
            mutate(num = row_number()) %>%
            ungroup() %>%
            mutate(testcd = case_when(nchar(as.character(ARMCD)) <= 8 ~ as.character(ARMCD),
                                      num == 1 ~ as.character(substr(gsub("[[:space:]]+", "", ARMCD), 1, 8)),
                                      TRUE ~ as.character(paste0(substr(gsub("[[:space:]]+", "", ARMCD), 1, 7), num))))

  # Input dataset from ex
  ex_in <- ex %>%
           filter(EXDOSE > 0 & !grepl("PLACEBO", toupper(EXTRT))) %>%
           left_join(testcd %>% select(ARM, testcd), by = c("EXTRT" = "ARM")) %>%
           left_join(cat_df %>% select(TRT, cat), by = c("EXTRT" = "TRT")) %>%
           left_join(scat_df %>% select(TRT, scat), by = c("EXTRT" = "TRT")) %>%
           left_join(visn_df %>% select(TRT, visn), by = c("EXTRT" = "TRT")) %>%
           left_join(spec_df %>% select(TRT, spec), by = c("EXTRT" = "TRT")) %>%
           left_join(nam_df %>% select(TRT, nam), by = c("EXTRT" = "TRT"))

  # Creating rows for each tpt and visit
  df1 <- ex_in %>%
         left_join(tpt_df, by = c("EXTRT" = "TRT")) %>%
         left_join(tptn_df, by = c("EXTRT" = "TRT")) %>%
         mutate(visn = str_split(ifelse(!is.na(visn), visn, visn_default), "\\s*,\\s*"),
                tpt = str_split(ifelse(!is.na(tpt), tpt, tpt_default), "\\s*,\\s*"),
                tptn = str_split(ifelse(!is.na(tptn), tptn, tptn_default), "\\s*,\\s*"),
                spec = ifelse(!is.na(spec), as.character(spec), spec_default)) %>%
         unnest(cols = c(visn)) %>%
         unnest(cols = c(visn, tpt, tptn)) %>%
         mutate(tptn = as.numeric(tptn)) %>%
         arrange(USUBJID, EXTRT, visn, tptn) %>%
         group_by(USUBJID, EXTRT, visn) %>%
         mutate(join_n = row_number()) %>%
         ungroup() %>%
         select(USUBJID, EXTRT, EXSTDTC, visn, tpt, tptn, join_n, testcd, cat, scat, spec, nam)

  # Joining Unit values and respective Conversion Factors
  df2 <- df1 %>%
         left_join(unit_org_df, by = c("EXTRT" = "TRT")) %>%
         left_join(unit_std_df, by = c("EXTRT" = "TRT")) %>%
         mutate(unit_org = if_else(is.na(unit_org), "ng/mL", as.character(unit_org), missing = "ng/mL"),
                unit_std = if_else(is.na(unit_std), "ng/mL", as.character(unit_std), missing = "ng/mL"),
                visn = as.numeric(visn)) %>%
         left_join(pc_cf, by = c("unit_org", "unit_std")) %>%
         left_join(tv %>% select(VISIT, VISITNUM), by = c("visn" = "VISITNUM"))

  # Range values for each timepoint within each TRT to get random results
  df_list <- list()
  f_list <- list()

  if (nrow(tptn_df) > 0) {
    # Looping each TRT values
    for (trt_loop in tptn_df$TRT) {

      # Temporary dataframe to hold data for each TRT
      trt_df_temp <- tptn_df %>%
                     filter(TRT == trt_loop & !is.na(tptn)) %>%
                     mutate(n_tpt = str_count(tptn, ",") + 1)

      # Median among timepoints to map the maximum concentration value to it
      split_val <- ceiling((trt_df_temp$n_tpt/2) + 0.1)

      # Maximum range value for a concentration in ng/mL units
      sum_val <- 10000/split_val

      # Looping each TPT values
      for (n_loop in 1:trt_df_temp$n_tpt) {

        # Mapping min, max values for each timepoint based on sum_val
        if (n_loop <= split_val) {
          temp <- data.frame(join_n = n_loop,
                             TRT = trt_df_temp$TRT,
                             min = as.numeric(round((n_loop - 1) * sum_val)),
                             max = as.numeric(round(n_loop * sum_val)),
                             timediff = ifelse(n_loop <= 4, n_loop * 15, n_loop * 30)) %>%   # Mapping time difference value to derive PCDTC from EXSTDTC
                             mutate(eltm = case_when(timediff < 60 ~ paste0("PT", timediff, "M"),
                                                     timediff >= 60 ~ paste0("PT", timediff/60, "H")),
                                    min = ifelse(min == 0, 10, min))
        } else {
          temp <- data.frame(join_n = n_loop,
                             TRT = trt_df_temp$TRT,
                             min = as.numeric(round(((split_val - (n_loop - split_val)) - 1) * sum_val)),
                             max = as.numeric(round((split_val - (n_loop - split_val)) * sum_val)),
                             timediff = ifelse(n_loop <= 12, n_loop * 60, n_loop * 120)) %>%
                             mutate(eltm = paste0("PT", timediff/60, "H"))
        }

        df_list[[n_loop]] <- temp
      }

      f_list[[trt_loop]] <- bind_rows(df_list)
    }
  } else {
    f_list[[1]] <- data.frame(TRT = character(0))
  }

  # Default range values for TPTs
  res_in_default <- data.frame(TRT = c(NA),
                               join_n = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                               min = c(10, 2000, 4000, 6000, 8000, 6000, 4000, 2000, 10),
                               max = c(2000, 4000, 6000, 8000, 10000, 8000, 6000, 4000, 2000),
                               timediff = c(-15, 15, 30, 60, 120, 240, 360, 720, 1440),
                               eltm = c("-PT15M", "PT15M", "PT30M", "PT1H", "PT2H", "PT4H", "PT6H", "PT12H", "PT24H"))

  # min max values for each timepoint within each TRT
  res_in <- bind_rows(f_list) %>%
            group_by(TRT) %>%
            mutate(min = ifelse(n() == row_number(), 10, min)) %>%
            ungroup() %>%
            bind_rows(res_in_default)

  # Mapping min, max values for each TPT based on TRT
  df_trt <- df2 %>%
            left_join(res_in, by = c("EXTRT" = "TRT", "join_n" = "join_n"))

  # Mapping min, max default values for TRT which has no inputs
  df_wo_trt <- df_trt %>%
               filter(is.na(min)) %>%
               select(-min, -max, -timediff, -eltm) %>%
               left_join(res_in %>% filter(is.na(TRT)), by = c("join_n" = "join_n"))

  # Coinbining the input dataframes
  df3 <- bind_rows(df_trt %>% filter(!is.na(min)), df_wo_trt)

  # Mapping result values based on min, max values and including few NA values
  df4 <- df3 %>%
         mutate(res = round(with_seed(seed, runif(n(), min, max))),
                res = ifelse(grepl("PRE DOSE", toupper(tpt)) | grepl("PRE-DOSE", toupper(tpt)), with_seed(seed, sample(c("<LLOQ", "BLOQ"), n(), replace = TRUE)), res),
                res = ifelse(row_number() %in% c(with_seed(seed, sample(1:n(), n()/10))), NA, res))

  # Mapping general variables and result variables
  df5 <- df4 %>%
         mutate(STUDYID = studyid,
                DOMAIN = domain,
                PCGRPID = paste0(USUBJID, "-", VISIT),
                PCTESTCD = as.character(testcd),
                PCTEST = EXTRT,
                PCCAT = as.character(cat),
                PCSCAT = as.character(scat),
                PCORRES = case_when(unit_org == "ng/mL" ~ as.character(res),
                                    unit_org == "ug/mL" ~ ifelse(is.na(suppressWarnings(as.numeric(res))), as.character(res), as.character(suppressWarnings(as.numeric(res)) * 0.001))),
                PCORRESU = ifelse(!is.na(PCORRES) & PCORRES != "", unit_org, NA_character_),
                PCSTRESC = case_when(unit_std == "ng/mL" ~ as.character(res),
                                     unit_std == "ug/mL" ~ ifelse(is.na(suppressWarnings(as.numeric(res))), as.character(res), as.character(suppressWarnings(as.numeric(res)) * 0.001))),
                PCSTRESN = suppressWarnings(as.numeric(PCSTRESC)),
                PCSTRESU = ifelse(!is.na(PCSTRESC) & PCSTRESC != "", unit_std, NA_character_),
                PCSTAT = if_else(is.na(PCORRES) | PCORRES == "", "NOT DONE", "", missing = "NOT DONE"),
                PCREASND = if_else(is.na(PCORRES) | PCORRES == "", "UNKNOWN", "", missing = "UNKNOWN"),
                PCNAM = as.character(nam),
                PCSPEC = as.character(spec),
                PCLLOQ = as.numeric(case_when(unit_std == "ng/mL" ~ 10,
                                              unit_std == "ug/mL" ~ 0.01)),
                VISITNUM = as.numeric(visn),
                timediff = ifelse(grepl("PRE DOSE", toupper(tpt)) | grepl("PRE-DOSE", toupper(tpt)), -15, timediff),
                PCDTC =  case_when(nchar(as.character(EXSTDTC)) == 10 ~ suppressWarnings(format(ymd_hm(paste0(substr(EXSTDTC, 1, 10), " 08:00")) + minutes(timediff), "%Y-%m-%dT%H:%M")),
                                   nchar(as.character(EXSTDTC)) >= 16 ~ suppressWarnings(format(ymd_hm(substr(str_replace(EXSTDTC, "T", " ") , 1, 16)) + minutes(timediff), "%Y-%m-%dT%H:%M")),
                                   TRUE ~ NA_character_),
                PCTPT = as.character(tpt),
                PCTPTNUM = suppressWarnings(as.numeric(tptn)),
                PCELTM = ifelse(grepl("PRE DOSE", toupper(tpt)) | grepl("PRE-DOSE", toupper(tpt)), "-PT15M", as.character(eltm)),
                PCTPTREF = "MOST RECENT DOSE",
                PCRFTDTC = case_when(nchar(as.character(EXSTDTC)) == 10 ~ suppressWarnings(format(ymd_hm(paste0(substr(EXSTDTC, 1, 10), " 08:00")), "%Y-%m-%dT%H:%M")),
                                     nchar(as.character(EXSTDTC)) >= 16 ~ suppressWarnings(format(ymd_hm(substr(str_replace(EXSTDTC, "T", " ") , 1, 16)), "%Y-%m-%dT%H:%M")),
                                     TRUE ~ NA_character_))

  # Mapping EPOCH variable
  df6 <- epoch(df = df5, dtc = "PCDTC")

  # Mapping --DY variable
  df7 <- stdy(df = df6, dtc = "PCDTC")

  # Mapping SEQ variable
  df8 <- seqnum(df = df7, sort = sort_seq)

  # Keeping only the Necessary variables
  df9 <- df8 %>%
         select(STUDYID, DOMAIN, USUBJID, PCSEQ, PCGRPID, PCTESTCD, PCTEST, PCCAT, PCSCAT, PCORRES, PCORRESU, PCSTRESC,
                PCSTRESN, PCSTRESU, PCSTAT, PCREASND, PCNAM, PCSPEC, PCLLOQ, VISITNUM, VISIT, EPOCH, PCDTC, PCDY,
                PCTPT, PCTPTNUM, PCELTM, PCTPTREF, PCRFTDTC)

  # Adding labels to the variables
  df10 <- apply_metadata(df9, pc_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    df10 <- df10 %>% select(-all_of(drop_vars))
  }

  # Final PC dataset
  assign("pc", df10, envir = .GlobalEnv)
 }
