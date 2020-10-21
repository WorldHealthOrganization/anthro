#' Weight-for-age zscore indicator
#'
#' @param weight numeric
#' @param age_in_days integer, the age in days.
#' @param age_in_months numeric, the age in months.
#' @param sex integer, the sex where 1 is male and 2 is female
#' @param flag_threshold numeric, a length 1 threshold. If the absolute value of the z-score is greater than
#'        this parameter, the z-score gets flagged in the resulting data frame.
#' @param growthstandards data.frame, the growstandards table for the weight-for-age indicator.
#' Do not change unless you know what you are doing.
#' @include z-score-helper.R
#' @noRd
anthro_zscore_weight_for_age <-
  function(weight, age_in_days, age_in_months, sex, oedema,
           flag_threshold = c(-6, 5),
           growthstandards = growthstandards_weianthro) {
    anthro_zscore_adjusted("wei", weight, age_in_days, age_in_months, sex,
      growthstandards, flag_threshold,
      allowed_age_range = c(0, 1856),
      !(oedema %in% "y")
    )
  }
