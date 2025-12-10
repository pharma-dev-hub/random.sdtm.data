#' Package imports

#  Pipes & deterministic randomness
#' @importFrom magrittr %>%
#' @importFrom withr with_seed

#  dplyr verbs & helpers
#  Core data verbs
#' @importFrom dplyr arrange group_by mutate ungroup filter left_join select
#' @importFrom dplyr row_number bind_rows distinct lead summarise rowwise
#' @importFrom dplyr case_when n if_else slice
#' @importFrom dplyr full_join sample_frac

#  Helpers used in sort expressions and tidyselect
#' @importFrom dplyr desc all_of

# Legacy helper used in rse(); consider refactoring away (superseded in dplyr >= 1.0)
#' @importFrom dplyr do

#  purrr mapping over vectors/rows
#' @importFrom purrr map map2 pmap pmap_chr

#  rlang (tidy eval helpers)
#' @importFrom rlang parse_exprs sym syms

#  stats
#' @importFrom stats rnorm na.omit lm coef runif

#  lubridate (date parsing & arithmetic)
#' @importFrom lubridate ymd_hm minutes today ymd days

#  stringr (string splitting/extracting/replacing)
#' @importFrom stringr str_replace str_extract str_split str_count

#  tidyr (cross-products & list-column expansion)
#' @importFrom tidyr crossing unnest separate_rows

#  checkmate (validation)
#' @importFrom checkmate assert_data_frame

# read Excel files
#' @importFrom readxl read_xlsx

#  tibble (compact data frames)
#' @importFrom tibble tibble

# package-private env
.the <- new.env(parent = emptyenv())

# To avoid NOTEs from R CMD CHECK
utils::globalVariables(c(".", ":=", "ACTARM", "ACTARMCD", "ACTARMUD", "AEACN", "AEACNOTH", "AEBDSYCD", "AEBODSYS",
                         "AEDECOD", "AEENDTC", "AEENDY", "AEENRF", "AEENRTPT", "AEENTPT", "AEHLGT", "AEHLGTCD",
                         "AEHLT", "AEHLTCD", "AELLT", "AELLTCD", "AEMODIFY", "AEOUT", "AEPATT", "AEPTCD", "AEREL",
                         "AESEQ", "AESER", "AESEV", "AESOC", "AESOCCD", "AESPID", "AESTDTC", "AESTDY", "AETERM",
                         "AGE", "AGEU", "ARM", "ARMCD", "ARMCD_8", "ARMNRS", "BRTHDTC", "CDISC.Submission.Value",
                         "COUNTRY", "Code", "Code.x", "Code.y", "Codelist.Name", "DMDTC", "DMDY", "DOMAIN", "DSCAT",
                         "DSDECOD", "DSSEQ", "DSSTDTC", "DSSTDY", "DSTERM", "DTHDTC", "DTHFL", "DVCAT", "DVDECOD",
                         "DVENDTC", "DVENDY", "DVSEQ", "DVSTDTC", "DVSTDY", "DVTERM", "ECDOSE", "ECDOSFRM", "ECDOSFRQ",
                         "ECDOSU", "ECENDTC", "ECENDY", "ECLNKID", "ECMOOD", "ECOCCUR", "ECPRESP", "ECROUTE", "ECSEQ",
                         "ECSTDTC", "ECSTDY", "ECTRT", "EGCAT", "EGDTC", "EGDY", "EGGRPID", "EGLOBXFL", "EGORRES",
                         "EGORRESU", "EGPOS", "EGREASND", "EGSCAT", "EGSEQ", "EGSTAT", "EGSTRESC", "EGSTRESN",
                         "EGSTRESU", "EGTEST", "EGTESTCD", "EGTPT", "EGTPTNUM", "ELEMENT", "EPOCH", "ETCD", "ETHNIC",
                         "EXDOSE", "EXDOSFRM", "EXDOSFRQ", "EXDOSU", "EXENDTC", "EXENDY", "EXROUTE", "EXSEQ", "EXSTDTC",
                         "EXSTDY", "EXTRT", "HOCAT", "HOENDTC", "HOENDY", "HOOCCUR", "HOPRESP", "HOSCAT", "HOSEQ",
                         "HOSTDTC", "HOSTDY", "HOTERM", "IECAT", "IEDTC", "IEDY", "IEORRES", "IESCAT", "IESEQ",
                         "IESTRESC", "IETEST", "IETESTCD", "INVID", "INVNAM", "LBCAT", "LBDTC", "LBDY", "LBGRPID",
                         "LBLOBXFL", "LBMETHOD", "LBNRIND", "LBORNRHI", "LBORNRLO", "LBORRES", "LBORRESU", "LBREASND",
                         "LBSCAT", "LBSEQ", "LBSPEC", "LBSTAT", "LBSTNRHI", "LBSTNRLO", "LBSTRESC", "LBSTRESN",
                         "LBSTRESU", "LBTEST", "LBTESTCD", "PCCAT", "PCDTC", "PCDY", "PCELTM", "PCGRPID", "PCLLOQ",
                         "PCNAM", "PCORRES", "PCORRESU", "PCREASND", "PCRFTDTC", "PCSCAT", "PCSEQ", "PCSPEC", "PCSTAT",
                         "PCSTRESC", "PCSTRESN", "PCSTRESU", "PCTEST", "PCTESTCD", "PCTPT", "PCTPTNUM", "PCTPTREF",
                         "PPCAT", "PPDTC", "PPDY", "PPGRPID", "PPORRES", "PPORRESU", "PPREASND", "PPRFTDTC", "PPSCAT",
                         "PPSEQ", "PPSPEC", "PPSTAT", "PPSTRESC", "PPSTRESN", "PPSTRESU", "PPTEST", "PPTESTCD", "PRCAT",
                         "PRDECOD", "PRDOSE", "PRDOSFRM", "PRDOSFRQ", "PRDOSTXT", "PRDOSU", "PRENDTC", "PRENDY", "PRLAT",
                         "PRLOC", "PROCCUR", "PRPRESP", "PRROUTE", "PRSCAT", "PRSEQ", "PRSTDTC", "PRSTDY", "PRTRT",
                         "QN_VALS", "RACE", "RFENDTC", "RFICDTC", "RFPENDTC", "RFSTDTC", "RFXENDTC", "RFXSTDTC",
                         "SEENDTC", "SEQ", "SESTDTC", "SEX", "SITEID", "STUDYID", "SUBJID", "SVENDTC", "SVENDY",
                         "SVOCCUR", "SVSTDTC", "SVSTDY", "TABRANCH", "TAETORD", "TATRANS", "TEDUR", "TEENRL",
                         "TERM_INDEX", "TESTRL", "TIRL", "TIVERS", "TSPARM", "TSPARMCD", "TSSEQ", "TSVAL", "TSVALCD",
                         "TSVALNF", "TSVCDREF", "TSVCDVER", "TVENRL", "TVSTRL", "USUBJID", "VISIT", "VISITDY", "VISITNUM",
                         "VSCAT", "VSDTC", "VSDY", "VSGRPID", "VSLAT", "VSLOBXFL", "VSLOC", "VSORRES", "VSORRESU", "VSPOS",
                         "VSREASND", "VSSCAT", "VSSEQ", "VSSTAT", "VSSTRESC", "VSSTRESN", "VSSTRESU", "VSTEST", "VSTESTCD",
                         "VSTPT", "VSTPTNUM", "allowed", "auc", "auc_t_to_inf", "cat_visn", "clast", "conv_factor",
                         "cum_2", "death_event", "death_flag", "dec_cf", "dm", "dose_vec", "dosfrm_vec", "dosfrq_vec",
                         "dosu_vec", "elaps_t", "eltm", "ex", "fit", "get_cached_data", "grpid_vars", "invalid_units",
                         "join_dt", "join_n", "keep", "ln_conc", "moment_val", "n_pts", "n_sample", "next_dy", "next_num",
                         "paumc", "pc", "prev_dy", "prev_num", "ref", "ref.x", "ref.y", "res", "resu", "result", "result_c",
                         "result_n", "route_vec", "row_id", "scat_visn", "se", "slope", "sv", "ta", "test", "testcd",
                         "testcd_visn", "ti", "time_diff", "timediff", "tivers_f", "tpt_cmax", "tpt_obs", "tptn",
                         "tptn_obs", "tv", "ver", "ver.x", "ver.y"))

#' Set the study ID to be used across the package
#'
#' If `value` is missing, defaults to `"RAND-001"`.
#' @param value Single, non-empty character scalar. Defaults to `"RAND-001"`.
#'
#' @return Invisibly returns the value set.
#' @export
#'
#' @examples
#' set_studyid("NEWID")
#' get_studyid()
set_studyid <- function(value = "RAND-001") {
  stopifnot(is.character(value), length(value) == 1, nzchar(value))
  .the$studyid <- value
  invisible(value)
}

#' Get the current study ID
#'
#' @return Character scalar; `"RAND-001"` if not set.
#' @export
#'
#' @examples
#' get_studyid()
get_studyid <- function() {
  val <- .the$studyid
  if (is.null(val)) "RAND-001" else val
}

#' Set the seed value to be used across the package, it does not change the user's session RNG seed value
#'
#' If `value` is missing, defaults to `1234`.
#' @param value Single, non-empty numeric scalar. Defaults to `1234`.
#'
#' @return Invisibly returns the value set.
#' @export
#'
#' @examples
#' set_with_seed(2345)
#' get_with_seed()
set_with_seed <- function(value = 1234) {
  stopifnot(is.numeric(value), length(value) == 1, is.finite(value), !is.na(value))
  .the$seed <- value
  invisible(value)
}

#' Get the current seed value
#'
#' @return Numeric scalar; `1234` if not set.
#' @export
#'
#' @examples
#' get_with_seed()
get_with_seed <- function() {
  val <- .the$seed
  if (is.null(val)) 1234 else val
}

#' Default race probabilities for rdm()
#' @keywords internal
default_race_probs <- c(
  "WHITE" = 0.7,
  "BLACK OR AFRICAN AMERICAN" = 0.15,
  "ASIAN" = 0.1,
  "AMERICAN INDIAN OR ALASKA NATIVE" = 0.03,
  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = 0.02
)

#' Default Title value to be used in rts()
#' @keywords internal
title_default <- "A Phase 3, Randomized, Double-Blind Study to Evaluate the Efficacy and Safety of IMP in Subjects with Cancer"

#' Function to apply labels to the variables based on the input list
#'
#' @param data A data.frame in which variables to be labelled
#' @param metadata A named list of labels to be mapped to the variables
#'
#' @return A dataframe with Variables label mapped
#' @export
#'
#' @examples
#' df1 <- data.frame(USUBJID = c("01-001","01-002"),
#'                   AGE     = c(34, 45),
#'                   SEX     = c("M","F"),
#'                   stringsAsFactors = FALSE)
#'
#' # Example metadata: variable -> label
#' eg_metadata <- list(USUBJID = "Subject Identifier",
#'                     AGE     = "Age (years)",
#'                     SEX     = "Sex")
#'
#' # Apply labels
#' df2 <- apply_metadata(df1, eg_metadata)
#' # Inspect result
#' str(df2)
#'
apply_metadata <- function(data, metadata) {
  checkmate::assert_data_frame(data)
  checkmate::assert_list(metadata)

  for (var in names(metadata)) {
    if (var %in% names(data)) {
      attr(data[[var]], "label") <- metadata[[var]]
    }
  }

  data
}

#' Function to check if optional parameter in a function has required input or else creating dummy values
#'
#' @param opt_var Optional parameter in a function input to be checked
#' @param comp_var Parameter value which needs to be compared with Optional Parameter
#'
#' @return Throwing Error or Generating dummy values
#' @export
#'
#' @examples
#' #This function should be used inside a function to compare the input or to create dummy values
#' func1 <- function(testcd = c(), cat = c()) {
#' ...
#' cat <- opt_var_chk(opt_var = "cat", comp_var = "testcd")
#' ...
#' }
opt_var_chk <- function(opt_var, comp_var) {

  # Getting values of the provided parameters for comparison
  opt_val <- get(opt_var, envir = parent.frame())
  comp_val <- get(comp_var, envir = parent.frame())

  # If Optional parameter included, then No. of Optional parameter's value and Comparator value should be matched
  if (length(opt_val) > 0 & length(opt_val) != length(comp_val)) {
    stop(paste0("Error: No. of ", toupper(opt_var), " and ", toupper(comp_var), " provided not matched. Add NA values if any of the ", toupper(comp_var), " doesn't require ",toupper(opt_var)))
  }

  # Creation of dummy value for Optional parameter, if it is not included
  if (length(opt_val) == 0) {
    opt_var <- c(rep(NA, length(comp_val)))
  } else {
    opt_var <- opt_val
  }

  return(opt_var)
}

#' Function to create random numeric values within specific range, specific decimal with specified probability of NA values
#'
#' @param n No of random numeric results to be generated, typically no.of rows in a dataframe
#' @param mean Mean value to be used for random result value
#' @param sd Standard Deviation value to be used for random result value
#' @param min Minimum value to be used for random result value
#' @param max Maximum value to be used for random result value
#' @param dec Decimal value to be used for random result value
#' @param na_prob Probability of NA values as a result, Input values should range from 0 to 1, Default value is 0.1
#'
#' @return Random Numeric values based on the input values
#' @export
#'
#' @examples
#' df1 <- data.frame(USUBJID = c("101-01", "101-02", "101-03", "101-04", "101-05"),
#'                   result = c(NA))
#'
#' df1$result <- rand_res(n = nrow(df1),
#'                        mean = 40,
#'                        sd = 5,
#'                        min = 10,
#'                        max = 70,
#'                        dec = 2)
rand_res <- function(n, mean, sd, min, max, dec, na_prob = 0.1) {

  if (na_prob > 1 | na_prob < 0) {
    stop("Error: Input for na_prob should between 0 to 1.")
  }
  # Generate normal values
  values <- rnorm(n, mean = mean, sd = sd)

  # Clip values to a specific range
  if (length(min) > 0 | length(max) > 0) {
    values <- pmin(pmax(values, min), max)
  }

  # Round to decimal places
  if (length(dec) > 0) {
    values <- round(values, dec)
  }

  # Introduce missing values randomly
  missing_indices <- with_seed(get_with_seed(), sample(1:n, size = round(na_prob * n)))
  values[missing_indices] <- NA

  return(values)
}

#' Function to create random character values within specified character vector, with specified probability of NA values
#'
#' @param choices Input character vector which has the values to be used for Random Character result values
#' @param n No of random character results to be generated, typically no.of rows in a dataframe
#' @param na_prob Probability of NA values as a result, Input values should range from 0 to 1, Default value is 0.1
#'
#' @return Random Character values based on the input values
#' @export
#'
#' @examples
#' df1 <- data.frame(USUBJID = c("101-01", "101-02", "101-03", "101-04", "101-05"),
#'                   result = c(NA))
#'
#' df1$result <- rand_res_char(choices = c("LOW", "HIGH", "NORMAL", "ABNORMAL"),
#'                             n = nrow(df1))
rand_res_char <- function(choices = c(), n, na_prob = 0.1) {

  values <- with_seed(get_with_seed(), sample(choices, size = n, replace = TRUE))

  # Introduce missing values randomly
  missing_indices <- with_seed(get_with_seed(), sample(1:n, size = round(na_prob * n)))
  values[missing_indices] <- NA

  return(values)
}

#' Function to map Sequence Number variable
#'
#' @param df A data.frame in which Sequence Number variable to be mapped, --SEQ Variable name would be based on the value in DOMAIN variable of input dataframe
#' @param sort Specify the sort order to be used for mapping Sequence Number
#'
#' @return A data.frame with Sequence Number variable mapped
#' @export
#'
#' @examples
#' # Example - It would create a df with EGSEQ var, since the value in DOMAIN var is "EG"
#' # df2 <- seqnum(df = df1,
#' #        sort = c("STUDYID", "USUBJID", "VISITNUM", "EGTESTCD", "EGDTC", "desc(EGGRPID)"))
seqnum <- function(df, sort = c()) {

  var_name <- paste0(unique(df$DOMAIN), "SEQ")

  temp1 <- df %>%
           arrange(!!!parse_exprs(sort)) %>%
           group_by(USUBJID) %>%
           mutate(!!sym(var_name) := row_number()) %>%
           ungroup()

  return(temp1)
}

#' Function to map Epoch variable
#'
#' @param df A data.frame in which Epoch variable to be mapped
#' @param dtc Date Variable which would be used to map EPOCH
#'
#' @return A data.frame with Epoch variable mapped
#' @export
#'
#' @examples
#' # Mapping EPOCH variable based on "EGDTC" value for EG dataframe
#' # df2 <- epoch(df = df1, dtc = "EGDTC")
epoch <- function(df, dtc) {

  # Merging with SE to get EPOCH details
  temp1 <- merge(df, se, by = "USUBJID")

  # Getting latest EPOCH value for each date within each Subject
  temp2 <- temp1 %>%
           filter(as.Date(SESTDTC) <= as.Date(!!sym(dtc)) & as.Date(!!sym(dtc)) <= as.Date(SEENDTC)) %>%
           arrange(USUBJID, !!sym(dtc), -TAETORD) %>%
           group_by(USUBJID, !!sym(dtc)) %>%
           filter(row_number() == 1)

  # Joining the EPOCH to the input dataframe
  temp3 <- left_join(df, temp2 %>% select(USUBJID, !!sym(dtc), EPOCH), by = c("USUBJID", dtc))

  return(temp3)
}

#' Function to map Study Day variables
#'
#' @param df A data.frame in which Study Day variable to be mapped
#' @param dtc Date Variable which would be used to map Study Day values, Name of the Study Day variable would be based on input Date Variable name
#'
#' @return A data.frame with Study Day variable mapped
#' @export
#'
#' @examples
#' # This example would create a EGDY variable based on the "EGDTC" value
#' # df2 <- stdy(df = df2, dtc = "EGDTC")
stdy <- function(df, dtc) {

  # Creating --DY variable name
  dy_var <- gsub("DTC", "DY", dtc)

  # Joining RFSTDTC frpm DM and calculating --DY Values
  temp1 <- left_join(df, dm %>% select(USUBJID, RFSTDTC), by = c("USUBJID")) %>%
           mutate(!!sym(dy_var) := ifelse(!is.na(!!sym(dtc)) & length(!!sym(dtc)) > 0 & !is.na(RFSTDTC) & length(RFSTDTC) > 0,
                                         ifelse(!!sym(dtc) >= RFSTDTC, as.integer((as.Date(!!sym(dtc)) - as.Date(RFSTDTC))  + 1), as.integer(as.Date(!!sym(dtc)) - as.Date(RFSTDTC))),
                                         NA_integer_)) %>%
           select(-RFSTDTC)

  return(temp1)
}

#' Function to map --LOBXFL variable
#'
#' @param df A data.frame in which --LOBXFL variable to be mapped
#' @param dtc Date Variable which would be used to map --LOBXFL values
#' @param res_var Result variable to be considered for checking non-missing values
#' @param sort Sort order to be used for mapping --LOBXFL
#'
#' @return A data.frame with --LOBXFL variable mapped
#' @export
#'
#' @examples
#' # The below example would create EGLOBXFL variable
#' \dontrun{
#'  df2 <- lobxfl(df = df1,
#'                dtc = "EGDTC",
#'                res_var = "EGORRES",
#'                sort = c("USUBJID", "EGTESTCD", "EGTPT"))
#' }
lobxfl <- function(df, dtc, res_var, sort = c()) {

  # VLOBXFL variable name based on DOMAIN
  var_name <- paste0(unique(df$DOMAIN), "LOBXFL")

  # Removing dtc column from sort list and adding it at last to sort in descending order
  grp_var <- c(na.omit(gsub(dtc, NA, sort)))
  sort_in <- c(grp_var, c(paste0("-",dtc)))

  # Filtering only non-missing result values & records before Exposure date, Based on sort group assigning "Y" for the LOBXFL variable
  temp1 <- df %>%
           left_join(., dm %>%  select(USUBJID, RFXSTDTC), by = c("USUBJID")) %>%
           filter(!!sym(res_var) != "" | !is.na(!!sym(res_var))) %>%
           filter(!!sym(dtc) < RFXSTDTC) %>%
           arrange(!!!(sort_in)) %>%
           group_by(!!!syms(grp_var)) %>%
           filter(row_number() == n()) %>%
           mutate(!!sym(var_name) := "Y") %>%
           ungroup()

  # Joining the LOBXFL variable to the input dataframe based on the group variables
  temp2 <- left_join(df, temp1 %>% select(all_of(grp_var), !!sym(dtc), !!sym(var_name)), by = c(grp_var, dtc))

  return(temp2)
}

#' Function to get first non-missing character value
#'
#' @param ... Mention the character variables to get the non-missing first occurance value
#'
#' @return First non-missing character value from the mentioned variables
#' @export
#'
#' @examples
#' coalescec(NA_character_, "5.0")
#' coalescec("5", "5.0")
#'
coalescec <- function(...) {
  values <- list(...)
  for(val in values) {
    if(!is.na(val) & val != "") return(val)
  }
  return(NA_character_)
}
