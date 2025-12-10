# R/rpp.R
#' Create Random Pharmacokinetics Parameters (PP) Dataset
#'
#' @description
#' Creates a random SDTM PP (Pharmacokinetics Parameters) dataset following CDISC SDTM standards.
#' Findings. One record per PK parameter per time-concentration profile per modeling method per subject, Tabulation.
#' Function is designed to create values for only the following Tests: "Max Conc", "Time of CMAX Observation", "AUC All", "AUC to Last Nonzero Conc", "Lambda z", "Half-Life Lambda z", "AUC Infinity Obs", "Clearance", "Volume of Distribution", "AUMC to Last Nonzero Conc", "Mean Residence Time"
#' The following Permissible variables have NOT been mapped within the rpp function: PPANMETH, TAETORD, PPTPTREF, PPSTINT, PPENINT
#' Dependency datasets: dm, pc, ex, se
#'
#' @param grp_by Grouping Variables to be used to derive Pharmacokinetic Parameters from PC data. Default value is given as c("USUBJID", "VISIT").
#' @param cat Values to be mapped under PPCAT variable - Expected format: PPTESTCD|PPCAT value. Each value must be separated by a pipe (|).
#' @param scat Values to be mapped under PPSCAT variable - Expected format: PPTESTCD|PPSCAT value. Each value must be separated by a pipe (|).
#' @param drop_testcds Mention the PPTESTCD values those needs to be dropped from the dataset
#' @param sort_seq Sorting sequence to be used for PPSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "PPTESTCD", "PPTEST", "PPDTC"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped - Variabe names should be in UPPERCASE
#'
#' @return A data.frame with SDTM PP structure
#' @export
#'
#' @examples
#' \dontrun{
#' pp <- rpp()
#'
#' pp <- rpp(grp_by = c("USUBJID", "VISIT", "PCGRPID"),
#'           cat = c("AUCALL|Area Under Curve", "AUCLST|Area Under Curve",
#'                   "AUCIFO|Area Under Curve"),
#'           scat = c("AUCLST|Last non-zero Obs"),
#'           drop_testcds = c("MRT"))
#' }

rpp <- function(grp_by = c("USUBJID", "VISIT"),
                cat = c(),
                scat = c(),
                drop_testcds = c(),
                sort_seq = c("STUDYID", "USUBJID", "PPTESTCD", "PPTEST", "PPDTC"),
                drop_vars = c()) {

  # Metadata for the PP dataset
  pp_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "PPSEQ" = "Sequence Number",
                      "PPGRPID" = "Group ID",
                      "PPTESTCD" = "Parameter Short Name",
                      "PPTEST" = "Parameter Name",
                      "PPCAT" = "Parameter Category",
                      "PPSCAT" = "Parameter Subcategory",
                      "PPORRES" = "Result or Finding in Original Units",
                      "PPORRESU" = "Original Units",
                      "PPSTRESC" = "Character Result/Finding in Std Format",
                      "PPSTRESN" = "Numeric Result/Finding in Standard Units",
                      "PPSTRESU" = "Standard Units",
                      "PPSTAT" = "Completion Status",
                      "PPREASND" = "Reason Parameter Not Calculated",
                      "PPSPEC" = "Specimen Material Type",
                      "PPANMETH" = "Analysis Method",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "PPDTC" = "Date/Time of Parameter Calculations",
                      "PPDY" = "Study Day of Parameter Calculations",
                      "PPTPTREF" = "Time Point Reference",
                      "PPRFTDTC" = "Date/Time of Reference Point",
                      "PPSTINT" = "Planned Start of Assessment Interval",
                      "PPENINT" = "Planned End of Assessment Interval")

  # Logic checks
  # Checking the availability of the dependent datasets
  if (!exists("dm") | !exists("pc") | !exists("ex") | !exists("se")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, pc, ex, se.")
  }

  # Checking if necessary inputs are provided for cat
  if (!is.null(cat) & length(cat) > 0) {
    cat_in = cat
    cat_df <- as.data.frame(cat_in) %>%
              mutate(testcd = sapply(strsplit(as.character(cat_in), "\\|"), "[", 1),
                     cat = sapply(strsplit(as.character(cat_in), "\\|"), "[", 2))
  } else {
    cat_df <- data.frame(testcd = character(0), cat = character(0))
  }

  # Checking if necessary inputs are provided for scat
  if (!is.null(scat) & length(scat) > 0) {
    scat_in = scat
    scat_df <- as.data.frame(scat_in) %>%
               mutate(testcd = sapply(strsplit(as.character(scat_in), "\\|"), "[", 1),
                      scat = sapply(strsplit(as.character(scat_in), "\\|"), "[", 2))
  } else {
    scat_df <- data.frame(testcd = character(0), scat = character(0))
  }

  # Max Conc
  cmax <- pc %>%
          group_by(!!!parse_exprs(grp_by)) %>%
          filter(PCSTRESN == max(PCSTRESN, na.rm = TRUE)) %>%
          group_by(!!!parse_exprs(grp_by)) %>%
          filter(row_number() == 1) %>%
          ungroup() %>%
          mutate(res = PCSTRESN,
                 resu = PCSTRESU,
                 testcd = "CMAX",
                 test = "Max Conc",
                 tpt_cmax = PCTPTNUM)

  # Time of CMAX Observation
  tmax <- cmax %>%
          select(-res, -resu, -test, -testcd) %>%
          mutate(res = as.numeric(as.POSIXct(PCDTC, format = "%Y-%m-%dT%H:%M") - as.POSIXct(PCRFTDTC, format = "%Y-%m-%dT%H:%M"), units = "hours"),
                 resu = "h",
                 testcd = "TMAX",
                 test = "Time of CMAX Observation")

  # Imputing 24 hr timepoint value to use for AUCALL derivation
  pc_in <- pc %>%
           group_by(!!!parse_exprs(grp_by)) %>%
           filter(row_number() == 1) %>%
           mutate(PCSTRESN = 0,
                  PCDTC = suppressWarnings(format(ymd_hm(substr(str_replace(PCRFTDTC, "T", " ") , 1, 16)) + minutes(1440), "%Y-%m-%dT%H:%M")),
                  PCTPTNUM = 8,
                  PCTPT = "24 hr Post-Dose",
                  sort = 999) %>%
           bind_rows(pc %>% mutate(sort = 1)) %>%    #Keeping the original results, if 24 hr Timepoint is a collected observation
           group_by(!!!parse_exprs(grp_by)) %>%
           arrange(USUBJID, PCTESTCD, PCTEST, VISITNUM, PCDTC, sort) %>%
           distinct(USUBJID, PCTESTCD, PCTEST, VISITNUM, PCDTC, .keep_all = TRUE)

  # AUC All
  aucall <- pc_in %>%
            group_by(!!!parse_exprs(grp_by)) %>%
            mutate(elaps_t = as.numeric(as.POSIXct(PCDTC, format = "%Y-%m-%dT%H:%M") - as.POSIXct(PCRFTDTC, format = "%Y-%m-%dT%H:%M"), units = "hours"),
                   cum_2 = PCSTRESN + lead(PCSTRESN),
                   time_diff = lead(elaps_t) - elaps_t,
                   auc = (cum_2/2) * time_diff,
                   aucall = sum(auc, na.rm = TRUE)) %>%
            filter(row_number() == 1) %>%
            ungroup() %>%
            mutate(res = aucall,
                   resu = ifelse(PCSTRESU == "ng/mL", "h*ng/mL", ifelse(PCSTRESU == "ug/mL", "h*ug/mL", NA_character_)),
                   testcd = "AUCALL",
                   test = "AUC All")

  # AUC to Last Nonzero Conc
  auclst <-  pc %>%
             group_by(!!!parse_exprs(grp_by)) %>%
             mutate(elaps_t = as.numeric(as.POSIXct(PCDTC, format = "%Y-%m-%dT%H:%M") - as.POSIXct(PCRFTDTC, format = "%Y-%m-%dT%H:%M"), units = "hours"),
                    cum_2 = PCSTRESN + lead(PCSTRESN),
                    time_diff = lead(elaps_t) - elaps_t,
                    auc = (cum_2/2) * time_diff,
                    auclst = sum(auc, na.rm = TRUE)) %>%
             filter(row_number() == 1) %>%
             ungroup() %>%
             mutate(res = auclst,
                    resu = ifelse(PCSTRESU == "ng/mL", "h*ng/mL", ifelse(PCSTRESU == "ug/mL", "h*ug/mL", NA_character_)),
                    testcd = "AUCLST",
                    test = "AUC to Last Nonzero Conc")

  # LAMZ and LAMZHL derivations
  lamz_der <- pc %>%
              left_join(cmax %>% select(!!!parse_exprs(grp_by), tpt_cmax), by = grp_by) %>%
              filter(PCTPTNUM > tpt_cmax) %>%   # Considering post CMAX observations for LAMZ derivations
              group_by(!!!parse_exprs(grp_by)) %>%
              summarise(elaps_t = list(as.numeric(as.POSIXct(PCDTC, format = "%Y-%m-%dT%H:%M") - as.POSIXct(PCRFTDTC, format = "%Y-%m-%dT%H:%M"), units = "hours")),
                        ln_conc = list(log(PCSTRESN)),
                        .groups = "drop_last") %>%
              rowwise() %>%
              mutate(n_pts = length(ln_conc),
                     fit   = ifelse(n_pts >= 3, list(lm(ln_conc ~ elaps_t)), list(NULL)),
                     slope = ifelse(!is.null(fit), coef(fit)[2], NA_real_),
                     adj_r2= ifelse(!is.null(fit), summary(fit)$adj.r.squared, NA_real_),
                     lamz  = ifelse(!is.null(fit) && is.finite(slope) && slope < 0, -slope, NA_real_),
                     lamzhl= ifelse(is.finite(lamz), log(2)/lamz, NA_real_),
                     t_low = ifelse(n_pts > 0, min(elaps_t), NA_real_),
                     t_high= ifelse(n_pts > 0, max(elaps_t), NA_real_)) %>%
              ungroup() %>%
              left_join(pc %>% distinct(!!!parse_exprs(grp_by), .keep_all = TRUE), by = grp_by) %>%
              select(-elaps_t)

  # Lambda z
  lamz <- lamz_der %>%
          mutate(res = lamz,
                 resu = "/h",
                 testcd = "LAMZ",
                 test = "Lambda z")

  # Half-Life Lambda z
  lamzhl <- lamz_der %>%
            mutate(res = lamzhl,
                   resu = "h",
                   testcd = "LAMZHL",
                   test = "Half-Life Lambda z")

  # AUC Infinity Obs
  aucifo <- pc %>%
            filter(!is.na(PCSTRESN)) %>%
            group_by(!!!parse_exprs(grp_by)) %>%
            filter(row_number() == n()) %>%
            mutate(clast = PCSTRESN) %>%
            left_join(lamz %>% select(!!!parse_exprs(grp_by), lamz), by = grp_by) %>%
            left_join(auclst %>% select(!!!parse_exprs(grp_by), auclst), by = grp_by) %>%
            mutate(auc_t_to_inf = clast/lamz,
                   aucifo = auclst + auc_t_to_inf,
                   res = aucifo,
                   resu = ifelse(PCSTRESU == "ng/mL", "h*ng/mL", ifelse(PCSTRESU == "ug/mL", "h*ug/mL", NA_character_)),
                   testcd = "AUCIFO",
                   test = "AUC Infinity Obs")

  # Input data for Dose value
  ex_in <- ex %>%
           mutate(join_dt = case_when(nchar(as.character(EXSTDTC)) == 10 ~ suppressWarnings(format(ymd_hm(paste0(substr(EXSTDTC, 1, 10), " 08:00")), "%Y-%m-%dT%H:%M")),
                                      nchar(as.character(EXSTDTC)) >= 16 ~ suppressWarnings(format(ymd_hm(substr(str_replace(EXSTDTC, "T", " ") , 1, 16)), "%Y-%m-%dT%H:%M")),
                                      TRUE ~ NA_character_)) %>%
           distinct(USUBJID, join_dt, .keep_all = TRUE)

  # Clearance
  cl <- aucifo %>%
        select(-res, -resu, -test, -testcd) %>%
        left_join(ex_in %>%  select(USUBJID, join_dt, EXDOSE), by = c("USUBJID" = "USUBJID", "PCRFTDTC" = "join_dt")) %>%
        mutate(cl = EXDOSE/aucifo,
               res = cl,
               resu = "L/h",
               testcd = "CL",
               test = "Clearance")

  # Volume of Distribution
  vz <- lamz_der %>%
        left_join(cl %>% select(!!!parse_exprs(grp_by), cl), by = grp_by) %>%
        mutate(vz = cl/lamz,
               res = vz,
               resu = "L",
               testcd = "VZ",
               test = "Volume of Distribution")

  # AUMC to Last Nonzero Conc
  aumclst <-  pc %>%
              group_by(!!!parse_exprs(grp_by)) %>%
              mutate(elaps_t = as.numeric(as.POSIXct(PCDTC, format = "%Y-%m-%dT%H:%M") - as.POSIXct(PCRFTDTC, format = "%Y-%m-%dT%H:%M"), units = "hours"),
                     moment_val = elaps_t * PCSTRESN,
                     paumc = 0.5 * (moment_val + lead(moment_val)) * (lead(elaps_t) - elaps_t),
                     aumclst = sum(paumc, na.rm = TRUE)) %>%
              filter(row_number() == 1) %>%
              ungroup() %>%
              mutate(res = aumclst,
                     resu = ifelse(PCSTRESU == "ng/mL", "h2*ng/mL", ifelse(PCSTRESU == "ug/mL", "h2*ug/mL", NA_character_)),
                     testcd = "AUMCLST",
                     test = "AUMC to Last Nonzero Conc")

  # Mean Residence Time
  mrt <- aumclst %>%
         select(-res, -resu, - testcd, -test) %>%
         left_join(auclst %>% select(!!!parse_exprs(grp_by), auclst), by = grp_by) %>%
         mutate(mrt = aumclst/auclst,
                res = mrt,
                resu = "h",
                testcd = "MRT",
                test = "Mean Residence Time")

  # Combining all the data
  cmb_df <- bind_rows(cmax, tmax, aucall, auclst, lamz, lamzhl, aucifo, cl, vz, aumclst, mrt) %>%
            left_join(cat_df, by = "testcd") %>%
            left_join(scat_df, by = "testcd") %>%
            select(-EPOCH)

  # Mapping general variables
  df1 <- cmb_df %>%
         mutate(STUDYID = get_studyid(),
                DOMAIN = "PP",
                PPGRPID = PCGRPID,
                PPTESTCD = testcd,
                PPTEST = test,
                PPCAT = cat,
                PPSCAT = scat,
                PPORRES = as.character(res),
                PPORRESU = resu,
                PPSTRESC = as.character(res),
                PPSTRESN = as.numeric(res),
                PPSTRESU = resu,
                PPSTAT = if_else(is.na(PPORRES) | PPORRES == "", "NOT DONE", "", missing = "NOT DONE"),
                PPREASND = if_else(is.na(PPORRES) | PPORRES == "", "UNKNOWN", "", missing = "UNKNOWN"),
                PPSPEC = ifelse("PCSPEC" %in% names(cmb_df), PCSPEC, NA_character_),
                PPDTC = format(today(), "%Y-%m-%d"),
                PPRFTDTC = ifelse("PCRFTDTC" %in% names(cmb_df), PCRFTDTC, NA_character_)) %>%
         filter(!(PPTESTCD %in% drop_testcds))

  # Mapping EPOCH variable
  df2 <- epoch(df = df1, dtc = "PPDTC")

  # Mapping --DY variable
  df3 <- stdy(df = df2, dtc = "PPDTC")

  # Mapping SEQ variable
  df4 <- seqnum(df = df3, sort = sort_seq)

  # Keeping only the Necessary variables
  df5 <- df4 %>%
         select(STUDYID, DOMAIN, USUBJID, PPSEQ, PPGRPID, PPTESTCD, PPTEST, PPCAT, PPSCAT, PPORRES,
                PPORRESU, PPSTRESC, PPSTRESN, PPSTRESU, PPSTAT, PPREASND, PPSPEC, EPOCH, PPDTC, PPDY, PPRFTDTC)

  # Adding labels to the variables
  pp <- apply_metadata(df5, pp_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    pp <- pp %>% select(-all_of(drop_vars))
  }

  # Final PP dataset
  return(pp)
}
