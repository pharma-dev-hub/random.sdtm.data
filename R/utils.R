library(dplyr)
library(tidyr)
library(purrr)
library(checkmate)
library(rlang)
library(tibble)
library(lubridate)
library(stringr)

# Global requirement
studyid <- "AD2025"
set.seed(2025)

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

# Function to check if optional variable has required input or else creating dummy values
opt_var_chk <- function(opt_var, comp_var){

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

# Function to create random values within specific range, specific decimal with specified probability of NA values
rand_res <- function(n, mean, sd, min, max, dec, na_prob = 0.1) {

  set.seed(1122)  # for reproducibility

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
  missing_indices <- sample(1:n, size = round(na_prob * n))  # 10% missing
  values[missing_indices] <- NA

  return(values)
}

# Function to map Sequence variable
seqnum <- function(df, sort = c()) {

  var_name <- paste0(unique(df$DOMAIN), "SEQ")

  temp1 <- df %>%
    arrange(!!!parse_exprs(sort)) %>%
    group_by(USUBJID) %>%
    mutate(!!sym(var_name) := row_number()) %>%
    ungroup()

  return(temp1)
}

# Function to map Epoch variable
epoch <- function(df, dtc) {

  # Merging with SE
  temp1 <- merge(df, se, by = "USUBJID")

  # Getting latest EPOCH value for each date within each Subject
  temp2 <- temp1 %>%
    filter(SESTDTC <= !!sym(dtc) & !!sym(dtc) <= SEENDTC) %>%
    arrange(USUBJID, !!sym(dtc), -TAETORD) %>%
    group_by(USUBJID, !!sym(dtc)) %>%
    filter(row_number() == 1)

  # Joining the EPOCH to the input dataframe
  temp3 <- left_join(df, temp2 %>% select(USUBJID, !!sym(dtc), EPOCH), by = c("USUBJID", dtc))

  return(temp3)
}

# Function to map Study Day variables
stdy <- function(df, dtc) {

  # Creating --DY variable name
  dy_var <- gsub("DTC", "DY", dtc)

  # Joining RFSTDTC frpm DM and calculating --DY Values
  temp1 <- left_join(df, dm %>% select(USUBJID, RFSTDTC), by = c("USUBJID")) %>%
    mutate(!!sym(dy_var) := ifelse(!is.na(!!sym(dtc)) & length(!!sym(dtc)) > 0 & !is.na(RFSTDTC) & length(RFSTDTC) > 0,
                                   ifelse(!!sym(dtc) >= RFSTDTC, as.integer((as.Date(!!sym(dtc)) - as.Date(RFSTDTC))  + 1), as.integer(as.Date(!!sym(dtc)) - as.Date(RFSTDTC))),
                                   NA_integer_))

  return(temp1)
}

# Function to map LOBXFL variable
lobxfl <- function(df, dtc, res_var, sort = c()){

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


# Dummy SV creation

sv1 <- crossing(dm %>% select(USUBJID, RFSTDTC), tv) %>%
  mutate(SVOCCUR = "Y",
         SVSTDTC = RFSTDTC) %>%
  filter(row_number() %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
                             21, 22, 23, 24, 25, 26, 27,
                             31,
                             41, 42,
                             51, 52, 53,
                             61, 62, 63, 64, 65, 66,
                             71, 72, 73, 75, 79,
                             84, 85, 86,
                             92, 94, 96, 98, 99))

unsch <- data.frame(USUBJID = c("AD2025-00005", "AD2025-00005", "AD2025-00005", "AD2025-00007", "AD2025-00007", "AD2025-00008", "AD2025-00010"),
                    VISIT = c("Unscheduled", "Unscheduled", "Unscheduled", "Unscheduled", "Unscheduled", "Unscheduled", "Unscheduled"),
                    VISITNUM = c(99, 99, 99, 99, 99, 99, 99),
                    SVOCCUR = c("N", "Y", "Y", "N", "Y", "N", "Y"),
                    SVSTDTC = as.Date(c("2026-02-02", "2025-02-12", "2026-12-02", "2026-09-02", "2023-10-02", "2026-02-22", "2022-02-02")))


sv <- bind_rows(sv1, unsch) %>%  select(USUBJID, VISIT, VISITNUM, SVOCCUR, SVSTDTC) %>% arrange(USUBJID, VISITNUM, SVSTDTC)

# Dummy SE creation
se1 <- crossing(dm, te %>%  select(-DOMAIN, -STUDYID))

se <- se1 %>%
  mutate(SESTDTC = case_when(ELEMENT == "Screening" ~ RFICDTC,
                             ELEMENT == "Treatment" ~ RFXSTDTC,
                             ELEMENT == "Follow-up" ~ RFENDTC),
         SEENDTC = case_when(ELEMENT == "Screening" ~ RFXSTDTC,
                             ELEMENT == "Treatment" ~ RFXENDTC,
                             ELEMENT == "Follow-up" ~ RFENDTC),
         EPOCH =   case_when(ELEMENT == "Screening" ~ "SCREENING",
                             ELEMENT == "Treatment" ~ "TRT",
                             ELEMENT == "Follow-up" ~ "FUP"),
         TAETORD = case_when(ELEMENT == "Screening" ~ 1,
                             ELEMENT == "Treatment" ~ 2,
                             ELEMENT == "Follow-up" ~ 3))
