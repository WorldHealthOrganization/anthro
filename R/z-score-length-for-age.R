#' Length-for-age zscore indicator
#'
#' @param lenhei numeric, the standardized length/height measure in cm. No further modification is done
#' @param age_in_days integer, the age in days.
#' @param sex integer, the sex where 1 is male and 2 is female
#' @param flag_threshold numeric, a length 1 threshold. If the absolute value of the z-score is greater than
#'        this parameter, the z-score gets flagged in the resulting data frame.
#' @param growthstandards data.frame, the growstandards table for the length-for-age indicator.
#' Do not change unless you know what you are doing.
#' @include z-score-helper.R
#' @noRd
anthro_zscore_length_for_age <-
  function(lenhei, age_in_days, sex, flag_threshold = 6,
             growthstandards = growthstandards_lenanthro) {
    anthro_zscore_adjusted("len", lenhei, age_in_days, sex,
      growthstandards, flag_threshold,
      allowed_age_range = c(0, 1856),
      zscore_fun = compute_zscore
    )
  }
