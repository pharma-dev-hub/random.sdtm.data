#' Get Cached SDTM Data
#'
#' @param dataname Character name of the dataset
#' @return A data.frame
#' @keywords internal
get_cached_data <- function(dataname) {
  checkmate::assert_string(dataname)

  cached_data_path <- system.file(
    "cached_data",
    paste0(dataname, ".rds"),
    package = "random.sdtm.data"
  )

  if (!file.exists(cached_data_path)) {
    stop(
      "Cached data for '", dataname,
      "' not found. Please rebuild cached data.",
      call. = FALSE
    )
  }

  readRDS(cached_data_path)
}

#' Apply Metadata to Dataset
#'
#' @param data A data.frame
#' @param metadata A list containing labels
#' @return A data.frame with labels applied
#' @keywords internal
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

#' Create Visit Schedule
#'
#' @param n_patients Number of patients
#' @param visits Character vector of visit names
#' @param visit_days Numeric vector of visit days
#' @return A data.frame with visit schedule
#' @keywords internal
create_visit_schedule <- function(n_patients, visits, visit_days) {
  checkmate::assert_count(n_patients, positive = TRUE)
  checkmate::assert_character(visits)
  checkmate::assert_numeric(visit_days)
  checkmate::assert_true(length(visits) == length(visit_days))

  expand.grid(
    USUBJID = seq_len(n_patients),
    VISIT = visits,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      VISITNUM = match(.data$VISIT, visits),
      VISITDY = visit_days[.data$VISITNUM]
    )
}
