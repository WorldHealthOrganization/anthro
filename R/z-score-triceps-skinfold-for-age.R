#' Triceps skinfold-for-age zscore indicator
#'
#' @param triskin numeric
#' @param age_in_days integer, the age in days.
#' @param age_in_months numeric, the age in months.
#' @param sex integer, the sex where 1 is male and 2 is female
#' @param flag_threshold numeric, a length 1 threshold. If the absolute value of the z-score is greater than
#'        this parameter, the z-score gets flagged in the resulting data frame.
#' @param growthstandards data.frame, the growstandards table for the weight-for-age indicator.
#' Do not change unless you know what you are doing.
#' @include z-score-helper.R
#' @noRd
anthro_zscore_triceps_skinfold_for_age <-
  function(triskin, age_in_days, age_in_months, sex, flag_threshold = 5,
           growthstandards = growthstandards_tsanthro) {
    anthro_zscore_adjusted(
      name = "ts",
      measure = triskin,
      age_in_days = age_in_days,
      age_in_months = age_in_months,
      sex = sex,
      growthstandards = growthstandards,
      flag_threshold = flag_threshold,
      allowed_age_range = c(91, 1856)
    )
  }
