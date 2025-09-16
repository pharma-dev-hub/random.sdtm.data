# R/sv.R
#' Create Subject Visits (SV) Dataset from DM and TV
#'
#' @description
#' Dynamically generates an SDTM-compliant SV dataset using subject-level data from DM and visit structure from TV.
#' Includes scheduled visits from TV and randomly adds unscheduled visits for a subset of subjects.
#'
#' @param dm A data.frame representing the DM dataset
#' @param tv A data.frame representing the TV dataset created by `rtv()`
#' @param add_unscheduled Logical, whether to add unscheduled visits (default: TRUE)
#' @param unsched_n Integer, number of subjects to assign unscheduled visits (default: 10)
#'
#' @return A data.frame with SDTM SV structure
#' @export
#'
#' @examples
#' sv <- rsv(dm, tv)

rsv <- function(dm, tv, add_unscheduled = TRUE, unsched_n = 10) {
  assert_data_frame(dm)
  assert_data_frame(tv)

  dm_sv <- dm %>%
    filter(!is.na(RFXSTDTC)) %>%
    mutate(RFXSTDTC = as.Date(RFXSTDTC),
           RFXENDTC = as.Date(RFXENDTC),
           DOMAIN = "SV")

  # Scheduled visits from TV
  sv_scheduled <- dm_sv %>%
    select(STUDYID, USUBJID, RFXSTDTC, DOMAIN) %>%
    crossing(tv %>% select(VISITNUM, VISIT, VISITDY)) %>%
    rowwise() %>%
    mutate(
      VISITNUM = signif(VISITNUM, 2),
      SVSTDTC = RFXSTDTC + days(VISITDY - 1),
      SVENDTC = SVSTDTC + days(1),
      SVSTDY = ifelse(SVSTDTC < RFXSTDTC, as.integer(SVSTDTC - RFXSTDTC),
                      as.integer(SVSTDTC - RFXSTDTC) + 1),
      SVENDY = ifelse(SVENDTC < RFXSTDTC, as.integer(SVENDTC - RFXSTDTC),
                      as.integer(SVENDTC - RFXSTDTC) + 1)
    ) %>%
    ungroup()

  # Prepare sorted VISITDY and VISITNUM pairs once
  tv_lookup <- tv %>% select(VISITDY, VISITNUM) %>% arrange(VISITDY)

  # Add unscheduled visits
  if (add_unscheduled) {
    unsched_n_actual <- sample(1:10, 1)
    unsched_subjects <- sample(unique(dm$USUBJID), size = min(unsched_n_actual, nrow(dm)))

    sv_unsched <- dm_sv %>%
      filter(USUBJID %in% unsched_subjects) %>%
      rowwise() %>%
      mutate(
        VISITDY = sample(10:90, 1),
        prev_dy = max(tv_lookup$VISITDY[tv_lookup$VISITDY < VISITDY], na.rm = TRUE),
        next_dy = min(tv_lookup$VISITDY[tv_lookup$VISITDY > VISITDY], na.rm = TRUE),
        prev_num = tv_lookup$VISITNUM[tv_lookup$VISITDY == prev_dy],
        next_num = tv_lookup$VISITNUM[tv_lookup$VISITDY == next_dy],
        VISITNUM = signif(round(prev_num + ((VISITDY - prev_dy) / (next_dy - prev_dy)) * (next_num - prev_num), 2), 2),
        VISIT = "Unscheduled",
        SVSTDTC = RFXSTDTC + days(VISITDY - 1),
        SVENDTC = SVSTDTC + days(1),
        SVSTDY = as.integer(SVSTDTC - RFXSTDTC + 1),
        SVENDY = as.integer(SVENDTC - RFXSTDTC + 1)
      ) %>%
      ungroup() %>%
      select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, SVSTDTC, SVENDTC, SVSTDY, SVENDY)
  } else {
    sv_unsched <- NULL
  }

  # Combine scheduled and unscheduled
  sv <- bind_rows(
    sv_scheduled %>% select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, SVSTDTC, SVENDTC, SVSTDY, SVENDY),
    sv_unsched
  ) %>%
    arrange(USUBJID, VISITNUM)

  # Assign epoch
  sv <- epoch(sv, dtc = "SVSTDTC")

  # Apply metadata
  sv <- apply_metadata(sv, list(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    VISITNUM = "Visit Number",
    VISIT    = "Visit Name",
    VISITDY  = "Planned Study Day of Visit",
    EPOCH    = "Epoch",
    SVSTDTC  = "Start Date/Time of Visit",
    SVENDTC  = "End Date/Time of Visit",
    SVSTDY   = "Study Day of Start of Visit",
    SVENDY   = "Study Day of End of Visit"
  ))

  # Order columns
  sv <- sv %>%
    select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, EPOCH,
           SVSTDTC, SVENDTC, SVSTDY, SVENDY)

  return(sv)
}

# Call the function to create the dataset
sv <- rsv(dm, tv)
