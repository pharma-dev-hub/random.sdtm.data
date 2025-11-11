
rpp <- function(domain = "PP",
                grp_by = c("USUBJID", "VISIT")
) {

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
  if (!exists("dm") | !exists("pc") | !exists("se") | !exists("sv")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, pc, se, sv.")
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
           bind_rows(pc %>% mutate(sort = 1)) %>%
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
                   resu = "ng.h/mL",
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
                    resu = "ng.h/mL",
                    testcd = "AUCLST",
                    test = "AUC to Last Nonzero Conc")

  # LAMZ and LAMZHL derivations
  lamz_der <- pc %>%
              left_join(cmax %>% select(!!!parse_exprs(grp_by), tpt_cmax), by = grp_by) %>%
              filter(PCTPTNUM > tpt_cmax) %>%
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
              ungroup()

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







  %>%
          mutate(ln_conc = log(PCSTRESN),
                 elaps_t = as.numeric(as.POSIXct(PCDTC, format = "%Y-%m-%dT%H:%M") - as.POSIXct(PCRFTDTC, format = "%Y-%m-%dT%H:%M"), units = "hours"),
                 lm_conc = lm(ln_conc ~ elaps_t, data = .)

                 )





                tmax = ,
                auclst = ,
                aucall = ,
                aucinf = ,
                lamz = ,
                lamzhl = ,
                cl = ,
                vz = ,
                mrt = ,
                aumc = ,
                cav = ,
                accum =
                )


}






