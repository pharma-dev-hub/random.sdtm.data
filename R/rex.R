# R/rex.R
#' Create Random Exposure (EX) Dataset
#'
#' @description
#' Creates a random SDTM EX (Exposure) dataset following CDISC SDTM standards.
#' Interventions. One record per protocol-specified study treatment, constant-dosing interval, per subject, Tabulation.
#' For each subjects, ACTARM values from DM would be mapped as EXTRT.
#' The following Permissible variables have NOT been mapped within the rex function: EXGRPID, EXREFID, EXSPID, EXLNKID, EXLNKGRP, EXCAT, EXSCAT, EXDOSTXT, EXDOSRGM, EXLOT, EXLOC, EXLAT, EXDIR, EXFAST, EXADJ, TAETORD, EXDUR, EXTPT, EXTPTNUM, EXELTM, EXTPTREF, EXRFTDTC.
#' Dependency datasets: dm, se
#'
#' @param n_dose No. of doses for each ARM. Input values should be given along with ARMCD value. Expected format: ARMCD|No. of Dose. If no inputs are given, No. of Dose will be 1 by default.
#' @param dose Dose value for each ARM and its repeated administration. Input values should be given along with ARMCD value. Expected format: ARMCD|Dose values separated by comma. Example: "PLA|100, 150, 200" - If "PLA" ARM has three different doses planned. If no inputs are given, Dose will be 100 by default.
#' @param dosu Dose Unit value for each ARM and its repeated administration. Input values should be given along with ARMCD value. Expected format: ARMCD|Dose Unit values separated by comma. Example: "PLA|mg, g, mg/kg" - If "PLA" ARM has three different doses planned. If no inputs are given, Dose Unit will be "mg" by default.
#' @param dosfrm Dose Form value for each ARM and its repeated administration. Input values should be given along with ARMCD value. Expected format: ARMCD|Dose Form values separated by comma. Example: "PLA|TABLET, TABLET, CAPSULE" - If "PLA" ARM has three different doses planned. If no inputs are given, Dose Form will be "TABLET" by default.
#' @param dosfrq Dose Frequency value for each ARM and its repeated administration. Input values should be given along with ARMCD value. Expected format: ARMCD|Dose Frequency values separated by comma. Example: "PLA|ONCE, ONCE, TWICE" - If "PLA" ARM has three different doses planned. If no inputs are given, Dose Frequency will be "ONCE" by default.
#' @param route Dose Route value for each ARM and its repeated administration. Input values should be given along with ARMCD value. Expected format: ARMCD|Dose Route values separated by comma. Example: "PLA|ORAL, ORAL, DIETARY" - If "PLA" ARM has three different doses planned. If no inputs are given, Dose Route will be "ORAL" by default.
#' @param sort_seq Sorting sequence to be used for EXSEQ mapping. Default value is given as c("STUDYID", "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC"), user can modify if required.
#' @param drop_vars List the Permissible variables with no values that needs to be dropped (optional) - Variabe names should be in UPPERCASE
#'
#' @return A data.frame with SDTM EX structure
#' @export
#'
#' @examples
#' \dontrun{
#' ex <- rex()
#'
#' ex <- rex(n_dose = c("PLA|2", "DER|4"),
#'           dose = c("ADA", "PLA|600", "DER|100, 150, 200, 400"),
#'           dosu = c("ADA", "PLA|mg", "DER|ml, mg/kg, mL, g"))
#' }

rex <- function(n_dose = c(),
                dose = c(),
                dosu = c(),
                dosfrm = c(),
                dosfrq = c(),
                route = c(),
                sort_seq = c("STUDYID", "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC"),
                drop_vars = c()) {

  # Metadata for the EX dataset
  ex_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "EXSEQ" = "Sequence Number",
                      "EXGRPID" = "Group ID",
                      "EXREFID" = "Reference ID",
                      "EXSPID" = "Sponsor-Defined Identifier",
                      "EXLNKID" = "Link ID",
                      "EXLNKGRP" = "Link Group ID",
                      "EXTRT" = "Name of Treatment",
                      "EXCAT" = "Category of Treatment",
                      "EXSCAT" = "Subcategory of Treatment",
                      "EXDOSE" = "Dose",
                      "EXDOSTXT" = "Dose Description",
                      "EXDOSU" = "Dose Units",
                      "EXDOSFRM" = "Dose Form",
                      "EXDOSFRQ" = "Dosing Frequency per Interval",
                      "EXDOSRGM" = "Intended Dose Regimen",
                      "EXROUTE" = "Route of Administration",
                      "EXLOT" = "Lot Number",
                      "EXLOC" = "Location of Dose Administration",
                      "EXLAT" = "Laterality",
                      "EXDIR" = "Directionality",
                      "EXFAST" = "Fasting Status",
                      "EXADJ" = "Reason for Dose Adjustment",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "EXSTDTC" = "Start Date/Time of Treatment",
                      "EXENDTC" = "End Date/Time of Treatment",
                      "EXSTDY" = "Study Day of Start of Treatment",
                      "EXENDY" = "Study Day of End of Treatment",
                      "EXDUR" = "Duration of Treatment",
                      "EXTPT" = "Planned Time Point Name",
                      "EXTPTNUM" = "Planned Time Point Number",
                      "EXELTM" = "Planned Elapsed Time from Time Point Ref",
                      "EXTPTREF" = "Time Point Reference",
                      "EXRFTDTC" = "Date/Time of Reference Time Point")

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
  df1 <- left_join(dm %>% filter(ARMNRS != "SCREEN FAILURE" & !is.na(RFXSTDTC)) %>% select(USUBJID, RFXSTDTC, RFXENDTC, ACTARM, ACTARMCD), n_dose_df, by = c("ACTARMCD")) %>%
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

  # Mapping general variables
  df3 <- df2 %>%
         group_by(USUBJID) %>%
         mutate(SEQ = row_number()) %>%
         ungroup() %>%
         mutate(STUDYID = get_studyid(),
                DOMAIN = "EX",
                EXTRT = ACTARM,
                EXDOSE = ifelse(!grepl("PLACEBO", toupper(EXTRT)), dose, 0),
                EXDOSU = dosu,
                EXDOSFRM = dosfrm,
                EXDOSFRQ = dosfrq,
                EXROUTE = route,
                EXSTDTC = as.Date(as.Date(RFXSTDTC) + (SEQ - 1)),
                EXENDTC = EXSTDTC,
                EXLNKID = paste0(USUBJID, "-", EXSTDTC))

  # Mapping EPOCH variable
  df4 <- epoch(df = df3, dtc = "EXSTDTC")

  # Mapping --DY variable
  df5 <- stdy(df = df4, dtc = "EXSTDTC")
  df6 <- stdy(df = df5, dtc = "EXENDTC")

  # Mapping SEQ variable
  df7 <- seqnum(df = df6, sort = sort_seq)

  # Keeping only the Necessary variables
  df8 <- df7 %>%
         select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXDOSFRQ,
                EXROUTE, EPOCH, EXSTDTC, EXENDTC, EXSTDY, EXENDY)

  # Adding labels to the variables
  ex <- apply_metadata(df8, ex_metadata)

  # Drop Variables
  if (length(drop_vars) > 0) {
    ex <- ex %>% select(-all_of(drop_vars))
  }

  # Final EX dataset
  return(ex)
}
