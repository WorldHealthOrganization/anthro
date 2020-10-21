#' Calculate z-scores for the eight anthropometric indicators
#'
#' @description
#'
#' This function calculates z-scores for the eight anthropometric indicators,
#' weight-for- age, length/height-for-age, weight-for-length/height,
#' body mass index (BMI)-for-age, head circumference-for-age,
#' arm circumference-for-age, triceps skinfold-for-age and subscapular
#' skinfold-for-age based on the WHO Child Growth Standards.
#'
#' @param sex A numeric or text variable containing gender information.
#'            If it is numeric, its values must be: 1 for males and 2 for
#'            females. If it is character, it must be "m" or "M" for males
#'            and "f" or "F" for females. No z-scores will be calculated
#'            if sex is missing.
#' @param age A numeric variable containing age information; age can be
#'            in either days or months (if optional argument is_age_in_month
#'            is set to TRUE). An exact age in days is expected and should
#'            not be rounded if age is in months. Age-related z-scores will
#'            NOT be calculated if age is missing (NA).
#' @param is_age_in_month A logical flag; if TRUE, variable age unit will be
#'            treated as months. The function converts it to days by dividing
#'            age by 30.4375 and rounding it to integer so that reference
#'            tables can be used. When unspecified, the default value FALSE
#'            is used and age unit is treated as days.
#' @param weight A numeric variable containing body weight information, which
#'               must be in kilograms. Weight-related z-scores are not
#'               calculated if body weight is missing.
#' @param lenhei A numeric variable containing length (recumbent length) or
#'               height (standing height) information, which must be in
#'               centimeters. Length/height-related z-scores will not be
#'               calculated if lenhei is missing. For children with age
#'               below 24 months (i.e. below 731 days) and standing height
#'               measured, the function converts it to recumbent length by
#'               adding 0.7 cm; and for children with age equal and above
#'               24 months and measured in recumbent length, the function
#'               converts it to standing height by subtracting 0.7 cm.
#'               This way all the z-scores calculated by this function are
#'               length-based for children below 24 months, and
#'               height-based otherwise. This converted length/height
#'               according to age is assigned to the variable clenhei in
#'               the resulting data.frame.
#' @param measure A character variable indicating whether recumbent length
#'                or standing height was measured for each observation.
#'                The values of this variable must be "L" or "l" for recumbent
#'                length, and "H" or "h" for standing height.
#'                Although it is highly recommended that this variable
#'                is provided according to the measurements taken in
#'                the survey, it is possible to run the analysis without
#'                specifying this variable. If unspecified, the default
#'                vector with all missing values is used. The function
#'                imputes the missing values according to the
#'                following algorithm:
#'                \itemize{
#'                \item If age is not missing, then it is recumbent length
#'                      if age below 24 months (731 days), and standing height
#'                      if age equal and above 24 months.
#'                \item If age is missing, then it is recumbent length if
#'                      measurement < 87 cm and standing height
#'                      if measurement >= 87 cm.
#'                }
#' @param headc A numeric variable containing head circumference information,
#'              which must be in centimeters. Head circumference-for-age
#'              z-scores are not calculated if head circumference is missing.
#' @param armc A numeric variable containing arm circumference information,
#'             which must be in centimeters. Arm circumference-for-age z-scores
#'             are not calculated if arm circumference is missing.
#' @param triskin A numeric variable containing triceps skinfold information,
#'             which must be in millimeters. Triceps skinfold-for-age z-scores
#'             are not calculated if triceps skinfold is missing.
#' @param subskin A numeric variable containing subscapular skinfold information,
#'             which must be in millimeters. Subscapular skinfold-for-age z-scores
#'             are not calculated if subscapular skinfold is missing.
#' @param oedema The values of this character variable must be "n", "N" or "2"
#'             for non-oedema, and "y", "Y", "1" for oedema. Although it is highly
#'             recommended that this variable is provided by the survey,
#'             it is possible to run
#'             the analysis without specifying this variable. If unspecified,
#'             the default vector of all "n" with values considered as
#'             non-oedema is used. Missing values will be
#'             treated as non-oedema. For oedema, weight related z-scores
#'             (zwei, zwfl and zbmi) are NOT calculated (set to missing),
#'             BUT they are treated as being < -3 SD in the weight-related
#'             indicator prevalence (\code{\link{anthro_prevalence}})
#'             estimation.
#'
#'
#' @return A data.frame with three types of columns. Columns starting with a
#' "c" are cleaned versions of the input arguments. Columns beginning with
#' a "z" are the respective z-scores and columns prefixed by a "f" indicate
#' if these z-scores are flagged (integers).
#' The number of rows is given by the length
#' of the input arguments.
#'
#' The following columns are returned:
#' \itemize{
#' \item{\code{clenhei}} converted length/height for deriving z-score
#' \item{\code{cbmi}} BMI value based on length/height given by clenhei
#'
#' \item{\code{zlen}} Length/Height-for-age z-score
#' \item{\code{flen}} 1, if \code{abs(zlen) > 6}
#'
#' \item{\code{zwei}} Weight-for-age z-score
#' \item{\code{fwei}} 1, if \code{zwei < -6} or \code{zwei > 5}
#'
#' \item{\code{zwfl}} Weight-for-length/height z-score
#' \item{\code{fwfl}} 1, if \code{abs(zwfl) > 5}
#'
#' \item{\code{zbmi}} BMI-for-age z-score
#' \item{\code{fbmi}} 1, if \code{abs(zbmi) > 5}
#'
#' \item{\code{zhc}} Head circumference-for-age z-score
#' \item{\code{fhc}} 1, if \code{abs(zhc) > 5}
#'
#' \item{\code{zac}} Arm circumference-for-age z-score
#' \item{\code{fac}} 1, if \code{abs(zac) > 5}
#'
#' \item{\code{zts}} Triceps skinfold-for-age z-score
#' \item{\code{fts}} 1, if \code{abs(zts) > 5}
#'
#' \item{\code{zss}} Subscapular skinfold-for-age z-score
#' \item{\code{fss}} 1, if \code{abs(zss) > 5}
#'
#' }
#'
#' If not all parameter values have equal length, parameter values will be
#' repeated to match the maximum length of all arguments except
#' \code{is_age_in_month} using \code{rep_len}. This happens without warnings.
#'
#' Z-scores are only computed for children younger than 60 months (age in months < 60)
#'
#' @references
#' WHO Multicentre Growth Reference Study Group (2006). WHO Child Growth Standards: Length/height-for-age, weight-for-age, weight-for-length, weight-for-height and body mass index-for-age: Methods and development.
#' Geneva: World Health Organization; pp 312. (web site: http://www.who.int/childgrowth/publications/en/ )
#'
#' WHO Multicentre Growth Reference Study Group (2007). WHO Child Growth Standards: Head circumference-for-age, arm circumference-for-age, triceps skinfold-for-age and subscapular skinfold-for-age: Methods and development.
#' Geneva: World Health Organization; pp 217. (web site: http://www.who.int/childgrowth/publications/en/ )
#'
#' @examples
#' # you can either use the function to compute zscores for specific values
#' anthro_zscores(sex = "f", age = 10, is_age_in_month = TRUE, weight = 10)
#'
#' # values will be recycled so not all input values need to be of the same length
#' anthro_zscores(sex = "f", age = c(10, 20, 30), weight = 10)
#'
#' # or use it with a compute dataset
#' \dontrun{
#' your_data_set <- read.csv("<your survey>.csv")
#' with(
#'   your_data_set,
#'   anthro_zscores(
#'     sex = sex, age = age_in_days,
#'     weight = weight, lenhei = lenhei
#'   )
#' )
#' }
#' @include anthro.R
#' @include assertions.R
#' @export
anthro_zscores <- function(sex,
                           age = NA_real_,
                           is_age_in_month = FALSE,
                           weight = NA_real_,
                           lenhei = NA_real_,
                           measure = NA_character_,
                           headc = NA_real_,
                           armc = NA_real_,
                           triskin = NA_real_,
                           subskin = NA_real_,
                           oedema = "n") {
  assert_logical(is_age_in_month)
  assert_length(is_age_in_month, 1L)
  assert_character_or_numeric(sex)
  assert_values_in_set(sex,
    allowed = c(
      "1", "2", "m",
      "f", "M", "F", NA_character_
    )
  )
  assert_numeric(age)
  assert_numeric(weight)
  assert_numeric(lenhei)
  assert_character(measure)
  assert_values_in_set(measure, allowed = c("l", "h", "L", "H", NA_character_))
  assert_numeric(headc)
  assert_numeric(armc)
  assert_numeric(triskin)
  assert_numeric(subskin)
  assert_values_in_set(oedema,
    allowed = c("n", "y", "N", "Y", "2", "1", NA_character_)
  )

  # make all input lengths equal
  max_len <- pmax(
    length(sex),
    length(age),
    length(weight),
    length(lenhei),
    length(measure),
    length(headc),
    length(armc),
    length(triskin),
    length(subskin),
    length(oedema)
  )
  sex <- rep_len(sex, length.out = max_len)
  age <- rep_len(age, length.out = max_len)
  weight <- rep_len(weight, length.out = max_len)
  lenhei <- rep_len(lenhei, length.out = max_len)
  measure <- rep_len(measure, length.out = max_len)
  headc <- rep_len(headc, length.out = max_len)
  armc <- rep_len(armc, length.out = max_len)
  triskin <- rep_len(triskin, length.out = max_len)
  subskin <- rep_len(subskin, length.out = max_len)
  oedema <- rep_len(oedema, length.out = max_len)

  # clean sex
  csex <- standardize_sex_var(sex)

  # clean measure
  cmeasure <- tolower(trimws(measure))
  cmeasure[!(is.na(cmeasure) | cmeasure %in% c("l", "h"))] <- NA_character_

  # clean oedema, we set all oedema not being "y" to "n"
  oedema <- standardize_oedema_var(oedema)

  age_in_days <- age_to_days(age, is_age_in_month = is_age_in_month)
  age_in_months <- age_to_months(age, is_age_in_month = is_age_in_month)

  # we consider a height measure for children younger than 9 months as
  # implausible
  measure_implausible <- !is.na(cmeasure) &
    !is.na(age_in_months) &
    cmeasure == "h" &
    age_in_months < 9
  cmeasure[measure_implausible] <- NA_character_

  clenhei <- adjust_lenhei(age_in_days, cmeasure, lenhei)

  cbmi <- weight / ((clenhei / 100)^2)
  cbind(
    clenhei,
    cbmi,
    cmeasure,
    csex,
    anthro_zscore_length_for_age(
      lenhei = clenhei,
      age_in_days = age_in_days,
      age_in_months = age_in_months,
      sex = csex
    ),
    anthro_zscore_weight_for_age(
      weight = weight,
      age_in_days = age_in_days,
      age_in_months = age_in_months,
      sex = csex,
      oedema = oedema
    ),
    anthro_zscore_weight_for_lenhei(
      weight = weight,
      lenhei = clenhei,
      lenhei_unit = cmeasure,
      age_in_days = age_in_days,
      age_in_months = age_in_months,
      sex = csex,
      oedema = oedema
    ),
    anthro_zscore_bmi_for_age(
      bmi = cbmi,
      age_in_days = age_in_days,
      age_in_months = age_in_months,
      sex = csex,
      oedema = oedema
    ),
    anthro_zscore_head_circumference_for_age(
      headc,
      age_in_days = age_in_days,
      age_in_months = age_in_months,
      sex = csex
    ),
    anthro_zscore_arm_circumference_for_age(
      armc = armc,
      age_in_days = age_in_days,
      age_in_months = age_in_months,
      sex = csex
    ),
    anthro_zscore_triceps_skinfold_for_age(
      triskin = triskin,
      age_in_days = age_in_days,
      age_in_months = age_in_months,
      sex = csex
    ),
    anthro_zscore_subscapular_skinfold_for_age(
      subskin = subskin,
      age_in_days = age_in_days,
      age_in_months = age_in_months,
      sex = csex
    ),
    stringsAsFactors = FALSE
  )
}
