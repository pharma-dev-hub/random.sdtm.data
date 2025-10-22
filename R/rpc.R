# R/rpc.R

# EXTRT/ACTARM Values should used while giving input for TPT/TPTN


rpc <- function(domain = "PC",
                tpt = c(),
                tptn = c(),
                unit_org = c(),
                unit_std = c(),
                sort_seq = c(),
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
  if (!exists("dm") | !exists("ex") | !exists("se") | !exists("sv")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, ex, se, sv.")
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

  # Default tpt, tptn values
  tpt_default = c("Pre-Dose, 15 min Post-Dose, 30 min Post-Dose, 1 hr Post-Dose, 2 hr Post-Dose, 4 hr Post-Dose, 6 hr Post-Dose, 12 hr Post-Dose, 24 hr Post-Dose")
  tptn_default = c("-1, 1, 2, 3, 4, 5, 6, 7, 8")
  n_tpt_default = 9

  # Input dataset from ex
  ex_in <- ex %>% filter(EXDOSE > 0 & toupper(EXTRT) != "PLACEBO")

  # Creating rows for each tpt
  df1 <- ex_in %>%
         left_join(tpt_df, by = c("EXTRT" = "TRT")) %>%
         left_join(tptn_df, by = c("EXTRT" = "TRT")) %>%
         mutate(tpt = str_split(ifelse(!is.na(tpt), tpt, tpt_default), "\\s*,\\s*"),
                tptn = str_split(ifelse(!is.na(tptn), tptn, tptn_default), "\\s*,\\s*")) %>%
         unnest(cols = c(tpt, tptn)) %>%
         select(USUBJID, EXTRT, tpt, tptn)

  # Joining Unit values and respective Conversion Factors
  df2 <- df1 %>%
         left_join(unit_org_df, by = c("EXTRT" = "TRT")) %>%
         left_join(unit_std_df, by = c("EXTRT" = "TRT")) %>%
         mutate(unit_org = if_else(is.na(unit_org), "ng/mL", as.character(unit_org), missing = "ng/mL"),
                unit_std = if_else(is.na(unit_std), "ng/mL", as.character(unit_std), missing = "ng/mL")) %>%
         left_join(pc_cf, by = c("unit_org", "unit_std"))






  # Input values to be used for generating random results
  res_inputs <- data.frame(unit_org = c("ng/mL", "ug/mL"),
                           mean = c(70000, 70, 170, 5.5, 70, 1.7, 1700, 25, 37, 98.5, 310, 80, 80, 18, 80, 8, 120, 12),
                           sd = c(8000, 8, 15, 0.5, 5, 0.2, 150, 4, 2, 3, 2, 10, 10, 3, 5, 0.5, 5, 0.5),
                           min = c(0, 0, 0, 0, 0, 0, 0, 0, 15, 59, 285, 20, 20, 5, 30, 3, 50, 5) ,
                           max = c(200000, 200, 250, 8, 100, 2.5, 2500, 60, 55, 131, 330, 220, 220, 45, 130, 13, 190, 19),
                           dec = c(1, 1, 0, 2, 2, 2, 2, 1, 1, 1, 2, 0, 0, 0, 0, 1, 0, 1),
                           dec_cf = c(1, 1, 0, 2, 2, 2, 2, 1, 1, 1, 2, 0, 0, 0, 0, 1, 0, 1), # Decimal factor to be used while converting values from org to std
                           na_prob = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))


  for (trt_loop in tptn_df$TRT) {

    trt_df_temp <- tptn_df %>%
                   filter(TRT == trt_loop) %>%
                   mutate(n_tpt = )
  }

  split_val <- ceiling((27/2) + 0.1)

  sum_val <- 10000/split_val

  df_list <- list()

  for (n_loop in 1:27) {

    if (n_loop <= split_val) {
      temp <- data.frame(num = n_loop,
                         min = as.numeric(round((n_loop - 1) * sum_val)),
                         max = as.numeric(round(n_loop * sum_val)))
    } else {
      temp <- data.frame(num = n_loop,
                         min = as.numeric(round(((split_val - (n_loop - split_val)) - 1) * sum_val)),
                         max = as.numeric(round((split_val - (n_loop - split_val)) * sum_val))) %>%
              mutate(min = ifelse(min == 0, 1, min))

    }

    df_list[[n_loop]] <- temp
  }

  res_in <- bind_rows(df_list)


  "Pre-Dose" 0 – 10 0 – 0.01
  "15 min Post-Dose" 50 – 5000 0.05 – 5
  "30 min Post-Dose" 100 – 8000 0.1 – 8
  "1 hr Post-Dose" 200 – 10000 0.2 – 10
  "2 hr Post-Dose" 150 – 9000 0.15 – 9
  "4 hr Post-Dose" 100 – 7000 0.1 – 7
  "6 hr Post-Dose" 50 – 5000 0.05 – 5
  "12 hr Post-Dose" 10 – 2000 0.01 – 2
  "24 hr Post-Dose" 1 – 500 0.001 – 0.5


}


