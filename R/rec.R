# R/rec.R
#' Create Random Exposure as Collected (EC) Dataset
#'
#' @description
#' Creates a random SDTM EC (Exposure as Collected) dataset following CDISC SDTM standards.
#' Interventions. One record per protocol-specified study treatment, collected-dosing interval, per subject, per mood, Tabulation.
#' For each subjects, ACTARM values from DM would be mapped as ECTRT.
#' The following Permissible variables have NOT been mapped within the rec function: ECGRPID, ECREFID, ECSPID, ECLNKGRP, ECCAT, ECSCAT, ECREASOC, ECDOSTXT, ECDOSTOT, ECDOSRGM, ECLOT, ECLOC, ECLAT, ECDIR, ECPORTOT, ECFAST, ECPSTRG, ECPSTRGU, ECADJ, TAETORD, ECDUR, ECTPT, ECTPTNUM, ECELTM, ECTPTREF, ECRFTDTC
#' Dependency datasets: dm, se
#'
#' @param domain By default, value has been set as "EC", user can modify it if needed but not recommended
#' @param n_dose No. of doses for each ARM. Input values should be given along with ARMCD value. Expected format: ARMCD|No. of Dose. If no inputs are given, No. of Dose will be 1 by default.
#' @param dose Dose value for each ARM and its repeated administration. Input values should be given along with ARMCD value. Expected format: ARMCD|Dose values separated by comma. Example: "PLA|100, 150, 200" - If "PLA" ARM has three different doses planned. If no inputs are given, Dose will be 100 by default.
#' @param dosu Dose Unit value for each ARM and its repeated administration. Input values should be given along with ARMCD value. Expected format: ARMCD|Dose Unit values separated by comma. Example: "PLA|mg, g, mg/kg" - If "PLA" ARM has three different doses planned. If no inputs are given, Dose Unit will be "mg" by default.
#' @param dosfrm Dose Form value for each ARM and its repeated administration. Input values should be given along with ARMCD value. Expected format: ARMCD|Dose Form values separated by comma. Example: "PLA|TABLET, TABLET, CAPSULE" - If "PLA" ARM has three different doses planned. If no inputs are given, Dose Form will be "TABLET" by default.
#' @param dosfrq Dose Frequency value for each ARM and its repeated administration. Input values should be given along with ARMCD value. Expected format: ARMCD|Dose Frequency values separated by comma. Example: "PLA|ONCE, ONCE, TWICE" - If "PLA" ARM has three different doses planned. If no inputs are given, Dose Frequency will be "ONCE" by default.
#' @param route Dose Route value for each ARM and its repeated administration. Input values should be given along with ARMCD value. Expected format: ARMCD|Dose Route values separated by comma. Example: "PLA|ORAL, ORAL, DIETARY" - If "PLA" ARM has three different doses planned. If no inputs are given, Dose Route will be "ORAL" by default.
#' @param sort_seq Sorting sequence to be used for ECSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "ECTRT", "desc(ECMOOD)", "ECSTDTC", "ECENDTC"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be in UPPERCASE
#'
#' @return A data.frame with SDTM EC structure
#' @export
#'
#' @examples
#' rec()
#'
#' rec(n_dose = c("PLA|2", "DER|4"),
#'     dose = c("ADA", "PLA|600", "DER|100, 150, 200, 400"),
#'     dosu = c("ADA", "PLA|mg", "DER|ml, mg/kg, mL, g"))
#'

rec <- function(domain = "EC",
                n_dose = c(),
                dose = c(),
                dosu = c(),
                dosfrm = c(),
                dosfrq = c(),
                route = c(),
                sort_seq = c("STUDYID", "USUBJID", "ECTRT", "desc(ECMOOD)", "ECSTDTC", "ECENDTC"),
                drop_vars = c()) {

  # Metadata for the EC dataset
  ec_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "ECSEQ" = "Sequence Number",
                      "ECGRPID" = "Group ID",
                      "ECREFID" = "Reference ID",
                      "ECSPID" = "Sponsor-Defined Identifier",
                      "ECLNKID" = "Link ID",
                      "ECLNKGRP" = "Link Group ID",
                      "ECTRT" = "Name of Treatment",
                      "ECMOOD" = "Mood",
                      "ECCAT" = "Category of Treatment",
                      "ECSCAT" = "Subcategory of Treatment",
                      "ECPRESP" = "Pre-Specified",
                      "ECOCCUR" = "Occurrence",
                      "ECREASOC" = "Reason for Occur Value",
                      "ECDOSE" = "Dose",
                      "ECDOSTXT" = "Dose Description",
                      "ECDOSU" = "Dose Units",
                      "ECDOSFRM" = "Dose Form",
                      "ECDOSFRQ" = "Dosing Frequency per Interval",
                      "ECDOSTOT" = "Total Daily Dose",
                      "ECDOSRGM" = "Intended Dose Regimen",
                      "ECROUTE" = "Route of Administration",
                      "ECLOT" = "Lot Number",
                      "ECLOC" = "Location of Dose Administration",
                      "ECLAT" = "Laterality",
                      "ECDIR" = "Directionality",
                      "ECPORTOT" = "Portion or Totality",
                      "ECFAST" = "Fasting Status",
                      "ECPSTRG" = "Pharmaceutical Strength",
                      "ECPSTRGU" = "Pharmaceutical Strength Units",
                      "ECADJ" = "Reason for Dose Adjustment",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "ECSTDTC" = "Start Date/Time of Treatment",
                      "ECENDTC" = "End Date/Time of Treatment",
                      "ECSTDY" = "Study Day of Start of Treatment",
                      "ECENDY" = "Study Day of End of Treatment",
                      "ECDUR" = "Duration of Treatment",
                      "ECTPT" = "Planned Time Point Name",
                      "ECTPTNUM" = "Planned Time Point Number",
                      "ECELTM" = "Planned Elapsed Time from Time Point Ref",
                      "ECTPTREF" = "Time Point Reference",
                      "ECRFTDTC" = "Date/Time of Reference Time Point")

  # Logic checks
  # Checking the availability of the dependent datasets
  if (!exists("dm") | !exists("se")) {
    stop("Error: One or more dependent dataset is not available. Make sure if the following datasets exist: dm, se.")
  }

  # Mapping No. of. Dose values to each ACTARM
  # Checking if necessary inputs are provided for n_dose
  if (!is.null(n_dose) & length(n_dose) > 0) {
    n_dose_in = n_dose
    n_dose_df <- as.data.frame(n_dose_in) %>%
                 mutate(ACTARMCD = sapply(strsplit(as.character(n_dose_in), "\\|"), "[", 1),
                        n_dose = suppressWarnings(as.numeric(sapply(strsplit(as.character(n_dose_in), "\\|"), "[", 2))))
  } else {
    n_dose_df <- data.frame(ACTARMCD = character(0), n_dose = numeric(0))
  }

  # Checking if necessary inputs are provided for dose
  if (!is.null(dose) & length(dose) > 0) {
    dose_in = dose
    dose_df <- as.data.frame(dose_in) %>%
               mutate(ACTARMCD = sapply(strsplit(as.character(dose_in), "\\|"), "[", 1),
                      dose = sapply(strsplit(as.character(dose_in), "\\|"), "[", 2))
  } else {
    dose_df <- data.frame(ACTARMCD = character(0), dose = numeric(0))
  }

  # Checking if necessary inputs are provided for dosu
  if (!is.null(dosu) & length(dosu) > 0) {
    dosu_in = dosu
    dosu_df <- as.data.frame(dosu_in) %>%
               mutate(ACTARMCD = sapply(strsplit(as.character(dosu_in), "\\|"), "[", 1),
                      dosu = sapply(strsplit(as.character(dosu_in), "\\|"), "[", 2))
  } else {
    dosu_df <- data.frame(ACTARMCD = character(0), dosu = numeric(0))
  }

  # Checking if necessary inputs are provided for dosfrm
  if (!is.null(dosfrm) & length(dosfrm) > 0) {
    dosfrm_in = dosfrm
    dosfrm_df <- as.data.frame(dosfrm_in) %>%
                 mutate(ACTARMCD = sapply(strsplit(as.character(dosfrm_in), "\\|"), "[", 1),
                        dosfrm = sapply(strsplit(as.character(dosfrm_in), "\\|"), "[", 2))
  } else {
    dosfrm_df <- data.frame(ACTARMCD = character(0), dosfrm = numeric(0))
  }

  # Checking if necessary inputs are provided for dosfrq
  if (!is.null(dosfrq) & length(dosfrq) > 0) {
    dosfrq_in = dosfrq
    dosfrq_df <- as.data.frame(dosfrq_in) %>%
                 mutate(ACTARMCD = sapply(strsplit(as.character(dosfrq_in), "\\|"), "[", 1),
                        dosfrq = sapply(strsplit(as.character(dosfrq_in), "\\|"), "[", 2))
  } else {
    dosfrq_df <- data.frame(ACTARMCD = character(0), dosfrq = numeric(0))
  }

  # Checking if necessary inputs are provided for route
  if (!is.null(route) & length(route) > 0) {
    route_in = route
    route_df <- as.data.frame(route_in) %>%
                mutate(ACTARMCD = sapply(strsplit(as.character(route_in), "\\|"), "[", 1),
                       route = sapply(strsplit(as.character(route_in), "\\|"), "[", 2))
  } else {
    route_df <- data.frame(ACTARMCD = character(0), route = numeric(0))
  }

  # Joining No.of Dose and Dose/Dosu value based on ACTARMCD to the DM subjects, Assigning default values
  df1 <- left_join(dm %>% filter(ARMNRS != "SCREEN FAILURE") %>% select(USUBJID, RFXSTDTC, RFXENDTC, ACTARM, ACTARMCD), n_dose_df, by = c("ACTARMCD")) %>%
         left_join(dose_df, by = c("ACTARMCD")) %>%
         left_join(dosu_df, by = c("ACTARMCD")) %>%
         left_join(dosfrm_df, by = c("ACTARMCD")) %>%
         left_join(dosfrq_df, by = c("ACTARMCD")) %>%
         left_join(route_df, by = c("ACTARMCD")) %>%
         mutate(n_dose = case_when(!is.na(n_dose) ~ n_dose,     #Mapping n_dose as 1 - default value
                                   TRUE ~ 1),
                dose = case_when(!is.na(dose) ~ as.character(dose),  #Mapping dose as 100 - default value
                                 TRUE ~ "100"),
                dosu = case_when(!is.na(dosu) ~ as.character(dosu),  #Mapping dosu as mg - default value
                                 TRUE ~ "mg"),
                dosfrm = case_when(!is.na(dosfrm) ~ as.character(dosfrm),  #Mapping dosfrm as TABLET - default value
                                   TRUE ~ "TABLET"),
                dosfrq = case_when(!is.na(dosfrq) ~ as.character(dosfrq),  #Mapping dosfrq as ONCE - default value
                                   TRUE ~ "ONCE"),
                route = case_when(!is.na(route) ~ as.character(route),  #Mapping route as ORAL - default value
                                  TRUE ~ "ORAL"))

  # Creating necords for each dose values and mapping 100 as dose/ mg as dosu if no values has been provided
  df2 <- df1 %>%
         mutate(dose_vec = str_split(dose, "\\s*,\\s*"),   # Split by comma, trim spaces
                dosu_vec = str_split(dosu, "\\s*,\\s*"),
                dosfrm_vec = str_split(dosfrm, "\\s*,\\s*"),
                dosfrq_vec = str_split(dosfrq, "\\s*,\\s*"),
                route_vec = str_split(route, "\\s*,\\s*"),

                dose_vec = map(dose_vec, ~ if (length(.x) == 1 && (is.na(.x) || .x == "")) character(0) else .x), # Treat NA or "" as no doses
                dosu_vec = map(dosu_vec, ~ if (length(.x) == 1 && (is.na(.x) || .x == "")) character(0) else .x),
                dosfrm_vec = map(dosfrm_vec, ~ if (length(.x) == 1 && (is.na(.x) || .x == "")) character(0) else .x),
                dosfrq_vec = map(dosfrq_vec, ~ if (length(.x) == 1 && (is.na(.x) || .x == "")) character(0) else .x),
                route_vec = map(route_vec, ~ if (length(.x) == 1 && (is.na(.x) || .x == "")) character(0) else .x),

                dose_vec = map2(dose_vec, n_dose, ~{ v <- .x; length(v) <- .y; v }),    # Pad/truncate to n_dose length
                dosu_vec = map2(dosu_vec, n_dose, ~{ v <- .x; length(v) <- .y; v }),
                dosfrm_vec = map2(dosfrm_vec, n_dose, ~{ v <- .x; length(v) <- .y; v }),
                dosfrq_vec = map2(dosfrq_vec, n_dose, ~{ v <- .x; length(v) <- .y; v }),
                route_vec = map2(route_vec, n_dose, ~{ v <- .x; length(v) <- .y; v })) %>%
         unnest(cols = c(dose_vec, dosu_vec, dosfrm_vec, dosfrq_vec, route_vec)) %>%  # Expand to rows based on dose values
         mutate(dose = case_when(is.na(suppressWarnings(as.numeric(dose_vec))) ~ 100,
                                 TRUE ~ as.numeric(dose_vec)),
                dosu = case_when(is.na(dosu_vec) ~ "mg",
                                 TRUE ~ dosu_vec),
                dosfrm = case_when(is.na(dosfrm_vec) ~ "TABLET",
                                   TRUE ~ dosfrm_vec),
                dosfrq = case_when(is.na(dosfrq_vec) ~ "ONCE",
                                   TRUE ~ dosfrq_vec),
                route = case_when(is.na(route_vec) ~ "ORAL",
                                  TRUE ~ route_vec)) %>%
         select(USUBJID, RFXSTDTC, RFXENDTC, ACTARM, ACTARMCD, dose, dosu, dosfrm, dosfrq, route)

  # Scheduled records dataset
  df_sch <- df2 %>%
            mutate(ECMOOD = "SCHEDULED")

  # Performed records dataset
  df_per <- df2 %>%
            group_by(USUBJID) %>%
            mutate(SEQ = row_number()) %>%
            ungroup() %>%
            mutate(ECMOOD = "PERFORMED",
                   ECPRESP = "Y",
                   ECOCCUR = ifelse(!is.na(RFXSTDTC), "Y", "N"),
                   ECSTDTC = as.Date(as.Date(RFXSTDTC) + (SEQ - 1)),
                   ECENDTC = ECSTDTC)

  # Combining Scheduled and Performed records and mapping general variables
  df3 <- bind_rows(df_sch, df_per) %>%
         mutate(STUDYID = studyid,
                DOMAIN = domain,
                ECTRT = ACTARM,
                ECDOSE = if_else(ECOCCUR == "N", NA_real_, ifelse(!toupper(ECTRT) %in% c("PLACEBO"), dose, 0), missing = dose),
                ECDOSU = if_else(ECOCCUR == "N", NA_character_, dosu, missing = dosu),
                ECDOSFRM = if_else(ECOCCUR == "N", NA_character_, dosfrm, missing = dosfrm),
                ECDOSFRQ = if_else(ECOCCUR == "N", NA_character_, dosfrq, missing = dosfrq),
                ECROUTE = if_else(ECOCCUR == "N", NA_character_, route, missing = route),
                ECLNKID = ifelse(ECMOOD == "PERFORMED", paste0(USUBJID, "-", ECSTDTC), NA))

  # Mapping EPOCH variable
  df4 <- epoch(df = df3, dtc = "ECSTDTC")

  # Mapping --DY variable
  df5 <- stdy(df = df4, dtc = "ECSTDTC")
  df6 <- stdy(df = df5, dtc = "ECENDTC")

  # Mapping SEQ variable
  df7 <- seqnum(df = df6, sort = sort_seq)

  # Keeping only the Necessary variables
  df8 <- df7 %>%
         select(STUDYID, DOMAIN, USUBJID, ECSEQ, ECLNKID, ECTRT, ECMOOD, ECPRESP, ECOCCUR, ECDOSE,
                ECDOSU, ECDOSFRM, ECDOSFRQ, ECROUTE, EPOCH, ECSTDTC, ECENDTC, ECSTDY, ECENDY)

  # Adding labels to the variables
  df9 <- apply_metadata(df8, ec_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    df9 <- df9 %>% select(-all_of(drop_vars))
  }

  # Final EC dataset
  assign("ec", df9, envir = .GlobalEnv)
}
