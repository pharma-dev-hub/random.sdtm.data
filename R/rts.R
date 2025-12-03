# R/rts.R
#' Create Random Trial Summary (TS) Dataset
#'
#' @description
#' Creates a random SDTM TS (Trial Summary) dataset following CDISC SDTM standards.
#' Trial Design. One record per trial summary parameter value, Tabulation.
#' The TSGRPID Permissible variable have NOT been mapped within the rts function.
#' SDTM Terminology 2025-03-28 is used.
#' Dependency datasets: dm, ta
#'
#' @param domain By default, value has been set as "TS", user can modify it if needed but not recommended
#' @param adapt TSVAL for records where TSPARMCD is 'ADAPT'. Default value is "N".
#' @param addon TSVAL for records where TSPARMCD is 'ADDON'. Default value is "N".
#' @param agemin TSVAL for records where TSPARMCD is 'AGEMIN'. Default value is 18.
#' @param agemax TSVAL for records where TSPARMCD is 'AGEMAX'. Default value is 70.
#' @param ageu Unit to be concatenated with AGEMIN/AGEMAX. Default value is "Y". Allowed values are "Y", "M", "D".
#' @param comptrt TSVAL for records where TSPARMCD is 'COMPTRT'
#' @param dcutdesc TSVAL for records where TSPARMCD is 'DCUTDESC'. Default value is "DATABASE LOCK".
#' @param dcutdtc TSVAL for records where TSPARMCD is 'DCUTDTC'. Default value is maximum value of RFPENDTC or System Date when all the RFPENDTC are missing.
#' @param dose TSVAL for records where TSPARMCD is 'DOSE'. Default value is 100.
#' @param dosfrm TSVAL for records where TSPARMCD is 'DOSFRM'. Default value is "TABLET".
#' @param dosfrq TSVAL for records where TSPARMCD is 'DOSFRQ'. Default value is "ONCE".
#' @param dosu TSVAL for records where TSPARMCD is 'DOSU'. Default value is "mg".
#' @param hltsubji TSVAL for records where TSPARMCD is 'HLTSUBJI'. Default value is "N".
#' @param intmodel TSVAL for records where TSPARMCD is 'INTMODEL'. Default value is "PARALLEL".
#' @param inttype TSVAL for records where TSPARMCD is 'INTTYPE'. Default value is "DRUG".
#' @param length TSVAL for records where TSPARMCD is 'LENGTH'. Default value is "P1Y".
#' @param objexp TSVAL for records where TSPARMCD is 'OBJEXP'. Default value is "To investigate potential biomarkers associated with response to Drug".
#' @param objprim TSVAL for records where TSPARMCD is 'OBJPRIM'. Default value is "To evaluate the efficacy of Drug".
#' @param objsec TSVAL for records where TSPARMCD is 'OBJSEC'. Default value is "To evaluate the safety and tolerability of Drug".
#' @param outmsexp TSVAL for records where TSPARMCD is 'OUTMSEXP'. Default value is "Change in biomarker levels from baseline to end of treatment".
#' @param outmspri TSVAL for records where TSPARMCD is 'OUTMSPRI'. Default value is "Change from baseline in primary clinical parameter".
#' @param outmssec TSVAL for records where TSPARMCD is 'OUTMSSEC'. Default value is "Incidence of Treatment-Emergent Adverse Events (TEAEs)".
#' @param pclas TSVAL for records where TSPARMCD is 'PCLAS'. Default value is "Antibiotics".
#' @param pdstind TSVAL for records where TSPARMCD is 'PDSTIND'. Default value is "N".
#' @param plansub TSVAL for records where TSPARMCD is 'PLANSUB'. Default value is 120.
#' @param random TSVAL for records where TSPARMCD is 'RANDOM'. Default value is "Y".
#' @param randqt TSVAL for records where TSPARMCD is 'RANDQT'. Default value is 0.75.
#' @param route TSVAL for records where TSPARMCD is 'ROUTE'. Default value is "ORAL".
#' @param sexpop TSVAL for records where TSPARMCD is 'SEXPOP'. Default value is "BOTH".
#' @param stoprule TSVAL for records where TSPARMCD is 'STOPRULE'. Default value is "Regulatory decision".
#' @param tblind TSVAL for records where TSPARMCD is 'TBLIND'. Default value is "DOUBLE BLIND".
#' @param tcntrl TSVAL for records where TSPARMCD is 'TCNTRL'. Default value is "PLACEBO".
#' @param tdigrp TSVAL for records where TSPARMCD is 'TDIGRP'. Default value is "Cancer".
#' @param therarea TSVAL for records where TSPARMCD is 'THERAREA'. Default value is "Oncology".
#' @param title TSVAL for records where TSPARMCD is 'TITLE'. Default value is "A Phase 3, Randomized, Double-Blind Study to Evaluate the Efficacy and Safety of IMP in Subjects with Cancer".
#' @param tphase TSVAL for records where TSPARMCD is 'TPHASE'. Default value is "PHASE III TRIAL".
#' @param trt TSVAL for records where TSPARMCD is 'TRT'. Default value is "IMP".
#' @param ttype TSVAL for records where TSPARMCD is 'TTYPE'. Default value is c("EFFICACY", "SAFETY", "TOLERABILITY").
#' @param sort_seq Sorting sequence to be used for TSSEQ mapping. Default value is given as c("STUDYID", "TSPARMCD", "TSVAL", "TSVALCD"), user can modify if required.
#' @param add_pars Add additional TSPARMCD/TSPARM/TSVAL records. Expected format: TSPARMCD|TSPARM|TSVAL text. Each value must be separated by a pipe (|).
#' @param drop_pars Add the TSPARMCD values, those TSPARMCD records needs to be dropped from the dataset
#'
#' @return A data.frame with SDTM TS structure
#' @export
#'
#' @examples
#' rts()
#'

rts <- function(domain = "TS",
                adapt = "N",
                addon = "N",
                agemin = 18,
                agemax = 70,
                ageu = "Y",
                comptrt = NA,
                dcutdesc = "DATABASE LOCK",
                dcutdtc = NULL,
                dose = 100,
                dosfrm = "TABLET",
                dosfrq = "ONCE",
                dosu = "mg",
                hltsubji = "N",
                intmodel = "PARALLEL",
                inttype = "DRUG",
                length = "P1Y",
                objexp = "To investigate potential biomarkers associated with response to Drug",
                objprim = "To evaluate the efficacy of Drug",
                objsec = "To evaluate the safety and tolerability of Drug",
                outmsexp = "Change in biomarker levels from baseline to end of treatment",
                outmspri = "Change from baseline in primary clinical parameter",
                outmssec = "Incidence of Treatment-Emergent Adverse Events (TEAEs)",
                pclas = "Antibiotics",
                pdstind = "N",
                plansub = 120,
                random = "Y",
                randqt = 0.75,
                route = "ORAL",
                sexpop = "BOTH",
                stoprule = "Regulatory decision",
                tblind = "DOUBLE BLIND",
                tcntrl = "PLACEBO",
                tdigrp = "Cancer",
                therarea = "Oncology",
                title = "A Phase 3, Randomized, Double-Blind Study to Evaluate the Efficacy and Safety of IMP in Subjects with Cancer",
                tphase = "PHASE III TRIAL",
                trt = "IMP",
                ttype = c("EFFICACY", "SAFETY", "TOLERABILITY"),
                sort_seq = c("STUDYID", "TSPARMCD", "TSVAL", "TSVALCD"),
                add_pars = NULL,
                drop_pars = c()
                ) {

  # Metadata for the TS dataset
  ts_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "TSSEQ" = "Sequence Number",
                      "TSGRPID" = "Group ID",
                      "TSPARMCD" = "Trial Summary Parameter Short Name",
                      "TSPARM" = "Trial Summary Parameter",
                      "TSVAL" = "Parameter Value",
                      "TSVALNF" = "Parameter Value Null Flavor",
                      "TSVALCD" = "Parameter Value Code",
                      "TSVCDREF" = "Name of the Reference Terminology",
                      "TSVCDVER" = "Version of the Reference Terminology")

  # Logic checks
  # Checking the availability of the dependent datasets
  if (!exists("dm") | !exists("ta")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, ta.")
  }

  # Checking if ageu has the allowed input
  if (!(ageu %in% c("Y", "M", "D"))) {
    stop("Error: Invalid input for ageu. Allowed values are: Y, M, D")
  }

  # Restriction for Objectives not to have text more than 200 characters
  if (nchar(objexp) > 200 | nchar(objprim) > 200 | nchar(objsec) > 200) {
    stop("Error: Text length is more than 200 characters. TSVAL for the following TSPARMs are restricted to have 200 characters: OBJEXP, OBJPRIM, OBJSEC.")
  }

  # Restriction for Outcomes/Title not to have text more than 200 characters
  if (nchar(outmsexp) > 200 | nchar(outmspri) > 200 | nchar(outmssec) > 200 | nchar(title) > 200) {
    stop("Error: Text length is more than 200 characters. TSVAL for the following TSPARMs are restricted to have 200 characters: OUTMSEXP, OUTMSPRI, OUTMSSEC, TITLE.")
  }

  # Check if input for RANDQT is provided with numeric values
  if (!is.numeric(randqt)) {
    stop("Error: RANDQT should be provided with numeric values")
  }

  # RANDQT values should be between 0 and 1
  if (is.numeric(randqt) & (randqt < 0 | randqt > 1)) {
    stop("Error: Input for RANDQT should between 0 to 1.")
  }

  # If dcutdtc is not provided - Mapping current date as dcutdtc if dm$RFPENDTC is all missing else mapping maximum of dm$RFPENDTC
  if (missing(dcutdtc) | is.null(dcutdtc)) {

    if (all(is.na(dm$RFPENDTC))) {
      dcutdtc = Sys.Date()
    } else {
      dcutdtc = max(as.Date(dm$RFPENDTC), na.rm = TRUE)
    }
  }

  if (all(is.na(dm$RFPENDTC))) {
    sendtc = Sys.Date()
  } else {
    sendtc = max(as.Date(dm$RFPENDTC), na.rm = TRUE)
  }

  # Study Start Date
  sstdtc <- min(as.Date(dm$RFICDTC), na.rm = TRUE)

  # If input for dcutdtc is provided, checking if it is in a valid format
  dtc_chk <- function(dtc) {
    if (inherits(dtc, "Date")) return(TRUE)
    !is.na(as.Date(dtc, format = "%Y-%m-%d"))
  }

  if (!dtc_chk(dcutdtc)) {
    stop("Error: Invalid input for the date variable dcutdtc. Expected formnat: YYYY-MM-DD")
  }

  # Planned Number of Arms
  narms <- length(unique(ta$ARM))

  # Option to keep the TSVAL variable in Character format instead of Factor
  options(stringsAsFactors = FALSE)

  # Mapping the TSPARMCD, TSPARM and TSVAL
  df1 <- rbind(data.frame(TSPARMCD = "ACTSUB",   TSPARM = "Actual Number of Subjects",            TSVAL = as.character(nrow(dm))),
               data.frame(TSPARMCD = "ADAPT",    TSPARM = "Adaptive Design",                      TSVAL = adapt),
               data.frame(TSPARMCD = "ADDON",    TSPARM = "Added on to Existing Treatments",      TSVAL = addon),
               data.frame(TSPARMCD = "AGEMAX",   TSPARM = "Planned Maximum Age of Subjects",      TSVAL = paste0("P", as.character(agemax), ageu)),
               data.frame(TSPARMCD = "AGEMIN",   TSPARM = "Planned Minimum Age of Subjects",      TSVAL = paste0("P", as.character(agemin), ageu)),
               data.frame(TSPARMCD = "COMPTRT",  TSPARM = "Comparative Treatment Name",           TSVAL = comptrt),
               data.frame(TSPARMCD = "DCUTDESC", TSPARM = "Data Cutoff Description",              TSVAL = dcutdesc),
               data.frame(TSPARMCD = "DCUTDTC",  TSPARM = "Data Cutoff Date",                     TSVAL = as.character(as.Date(dcutdtc))),
               data.frame(TSPARMCD = "DOSE",     TSPARM = "Dose per Administration",              TSVAL = as.character(dose)),
               data.frame(TSPARMCD = "DOSFRM",   TSPARM = "Dose Form",                            TSVAL = dosfrm),
               data.frame(TSPARMCD = "DOSFRQ",   TSPARM = "Dosing Frequency",                     TSVAL = dosfrq),
               data.frame(TSPARMCD = "DOSU",     TSPARM = "Dose Units",                           TSVAL = dosu),
               data.frame(TSPARMCD = "HLTSUBJI", TSPARM = "Healthy Subject Indicator",            TSVAL = hltsubji),
               data.frame(TSPARMCD = "INTMODEL", TSPARM = "Intervention Model",                   TSVAL = intmodel),
               data.frame(TSPARMCD = "INTTYPE",  TSPARM = "Intervention Type",                    TSVAL = inttype),
               data.frame(TSPARMCD = "LENGTH",   TSPARM = "Trial Length",                         TSVAL = length),
               data.frame(TSPARMCD = "NARMS",    TSPARM = "Planned Number of Arms",               TSVAL = as.character(narms)),
               data.frame(TSPARMCD = "OBJEXP",   TSPARM = "Trial Exploratory Objective",          TSVAL = objexp),
               data.frame(TSPARMCD = "OBJPRIM",  TSPARM = "Trial Primary Objective",              TSVAL = objprim),
               data.frame(TSPARMCD = "OBJSEC",   TSPARM = "Trial Secondary Objective",            TSVAL = objsec),
               data.frame(TSPARMCD = "OUTMSEXP", TSPARM = "Exploratory Outcome Measure",          TSVAL = outmsexp),
               data.frame(TSPARMCD = "OUTMSPRI", TSPARM = "Primary Outcome Measure",              TSVAL = outmspri),
               data.frame(TSPARMCD = "OUTMSSEC", TSPARM = "Secondary Outcome Measure",            TSVAL = outmssec),
               data.frame(TSPARMCD = "PCLAS",    TSPARM = "Pharmacologic Class",                  TSVAL = pclas),
               data.frame(TSPARMCD = "PDSTIND",  TSPARM = "Pediatric Study Indicator",            TSVAL = pdstind),
               data.frame(TSPARMCD = "PLANSUB",  TSPARM = "Planned Number of Subjects",           TSVAL = as.character(plansub)),
               data.frame(TSPARMCD = "RANDOM",   TSPARM = "Trial is Randomized",                  TSVAL = random),
               data.frame(TSPARMCD = "RANDQT",   TSPARM = "Randomization Quotient",               TSVAL = as.character(randqt)),
               data.frame(TSPARMCD = "ROUTE",    TSPARM = "Route of Administration",              TSVAL = route),
               data.frame(TSPARMCD = "SENDTC",   TSPARM = "Study End Date",                       TSVAL = as.character(as.Date(sendtc))),
               data.frame(TSPARMCD = "SEXPOP",   TSPARM = "Sex of Participants",                  TSVAL = sexpop),
               data.frame(TSPARMCD = "SSTDTC",   TSPARM = "Study Start Date",                     TSVAL = as.character(as.Date(sstdtc))),
               data.frame(TSPARMCD = "STOPRULE", TSPARM = "Study Stop Rules",                     TSVAL = stoprule),
               data.frame(TSPARMCD = "TBLIND",   TSPARM = "Trial Blinding Schema",                TSVAL = tblind),
               data.frame(TSPARMCD = "TCNTRL",   TSPARM = "Control Type",                         TSVAL = tcntrl),
               data.frame(TSPARMCD = "TDIGRP",   TSPARM = "Diagnosis Group",                      TSVAL = tdigrp),
               data.frame(TSPARMCD = "THERAREA", TSPARM = "Therapeutic Area",                     TSVAL = therarea),
               data.frame(TSPARMCD = "TITLE",    TSPARM = "Trial Title",                          TSVAL = title),
               data.frame(TSPARMCD = "TPHASE",   TSPARM = "Trial Phase Classification",           TSVAL = tphase),
               data.frame(TSPARMCD = "TRT",      TSPARM = "Investigational Therapy or Treatment", TSVAL = trt),
               data.frame(TSPARMCD = "TTYPE",    TSPARM = "Trial Type",                           TSVAL = ttype)
               )

  # Additional parameters to be added
  if (!is.null(add_pars) & length(add_pars) > 0) {

    add_pars_df <- as.data.frame(add_pars) %>%
                   mutate(TSPARMCD = sapply(strsplit(as.character(add_pars), "\\|"), "[", 1),
                          TSPARM = sapply(strsplit(as.character(add_pars), "\\|"), "[", 2),
                          TSVAL = sapply(strsplit(as.character(add_pars), "\\|"), "[", 3)) %>%
                   select(-add_pars)

    # Checking if necessary inputs are provided
    apply(add_pars_df, 1, function(row) {
      missing_vars <- names(add_pars_df)[is.na(row) | (row == "")]

      if (length(missing_vars) > 0) {
        stop(paste("Error: Invalid input for add_pars. Expected format: TSPARMCD|TSPARM|TSVAL text. Each value must be separated by a pipe (|). Missing value for the variable(s):", paste(missing_vars, collapse = ", ")))
      }
    })

    df2 <- rbind(df1, add_pars_df)
  } else {
    df2 <- df1
  }

  # Controlled Terminology to be used for TSVALCD mapping
  CT <- read_xlsx("inst/data/TS SDTM Terminology 2025-03-28.xlsx", .name_repair = "universal") %>%
           mutate(ref = "CDISC CT",
                  ver = "2025-03-28")

  TS_CT <- CT %>% filter(Codelist.Name != "Unit")

  UNIT_CT <- CT %>% filter(Codelist.Name == "Unit")

  # Mapping of General Variables and TSVALCD/TSVCDREF/TSVCDVER
  df3 <- df2 %>%
         mutate(STUDYID = studyid,
                DOMAIN = domain,
                TSVALNF = ifelse(TSVAL == "" | is.na(TSVAL) | nchar(TSVAL) == 0, "NA", NA_character_)) %>%
         left_join(TS_CT %>% select(Code, ref, ver, CDISC.Submission.Value), by = c("TSVAL" = "CDISC.Submission.Value")) %>%
         left_join(UNIT_CT %>% select(Code, ref, ver, CDISC.Submission.Value), by = c("TSVAL" = "CDISC.Submission.Value")) %>%
         rowwise() %>%
         mutate(TSVALCD = coalescec(as.character(Code.x), as.character(Code.y)),
                TSVCDREF = coalescec(as.character(ref.x), as.character(ref.y)),
                TSVCDVER = coalescec(as.character(ver.x), as.character(ver.y))) %>%
         ungroup() %>%
         mutate(TSVCDREF = ifelse(TSPARMCD %in% c("AGEMAX", "AGEMIN", "DCUTDTC", "SENDTC", "SSTDTC", "LENGTH"),"ISO 8601", TSVCDREF))

  # TSSEQ Mapping
  df4 <- df3 %>%
         arrange(!!!parse_exprs(sort_seq)) %>%
         group_by(TSPARMCD) %>%
         mutate(TSSEQ= row_number()) %>%
         ungroup() %>%
         filter(!(TSPARMCD %in% drop_pars))

  # Keeping only the Necessary variables
  df5 <- df4 %>%
         select(STUDYID, DOMAIN, TSSEQ, TSPARMCD, TSPARM, TSVAL, TSVALNF, TSVALCD, TSVCDREF, TSVCDVER)

  # Adding labels to the variables
  df6 <- apply_metadata(df5, ts_metadata)

  # Final TS dataset
  assign("ts", df6, envir = .GlobalEnv)

}
