#' Implements the non adjusted z-score
#'
#' @param y y
#' @param m m
#' @param l l
#' @param s s
#'
#' @references
#' http://www.who.int/childgrowth/standards/Chap_7.pdf?ua=1
#'
#' @noRd
compute_zscore <- function(y, m, l, s) {
  stopifnot(is.numeric(y), is.numeric(m), is.numeric(l), is.numeric(s))
  ( (y / m) ^ l - 1) / (s * l)
}

#' Implements the adjusted z-score
#'
#' @param y y
#' @param m m
#' @param l l
#' @param s s
#'
#' @references
#' http://www.who.int/childgrowth/standards/Chap_7.pdf?ua=1
#'
#' @noRd
compute_zscore_adjusted <- function(y, m, l, s) {
  stopifnot(is.numeric(y), is.numeric(m), is.numeric(l), is.numeric(s))
  calc_sd <- function(sd) m * ( (1 + l * s * sd) ^ (1 / l))

  zscore <- compute_zscore(y, m, l, s)
  SD3pos <- calc_sd(3)
  SD3neg <- calc_sd(-3)
  SD23pos <- SD3pos - calc_sd(2)
  SD23neg <- calc_sd(-2) - SD3neg

  # a type-safe way to use ifelse
  not_zscore_na <- !is.na(zscore)
  zscore_gt_3 <- not_zscore_na & zscore > 3
  zscore[zscore_gt_3] <- (3 + ( (y - SD3pos) / SD23pos))[zscore_gt_3]
  zscore_lt_3 <- not_zscore_na & zscore < -3
  zscore[zscore_lt_3] <- (-3 + ( (y - SD3neg) / SD23neg))[zscore_lt_3]

  zscore
}

apply_zscore_and_growthstandards <- function(zscore_fun, growthstandards,
                                             age_in_days, sex, measure) {
  n <- length(measure)
  age_in_days <- as.integer(round(age_in_days))
  input_df <- data.frame(measure, age_in_days, sex, ordering = seq_len(n))
  merged_df <- merge(input_df,
                     growthstandards,
                     by.x = c("age_in_days", "sex"), by.y = c("age", "sex"),
                     all.x = TRUE, sort = FALSE)
  merged_df <- merged_df[order(merged_df$ordering), ]

  y <- merged_df[["measure"]]
  m <- merged_df[["m"]]
  l <- merged_df[["l"]]
  s <- merged_df[["s"]]
  zscore <- zscore_fun(y, m, l, s)
  round(zscore, digits = 2L)
}

flag_zscore <- function(flag_threshold, score_name, zscore, valid_zscore) {
  stopifnot(length(flag_threshold) == 1L || length(flag_threshold) == 2L)

  zscore[!valid_zscore] <- NA_real_

  fzscore <- if (length(flag_threshold) == 1L) {
    abs(zscore) > flag_threshold
  } else {
    zscore < flag_threshold[1L] | zscore > flag_threshold[2L]
  }

  # we convert it to integer because of historical reasons
  fzscore <- as.integer(fzscore)

  result_df <- data.frame(score = zscore, fzscore = fzscore)
  colnames(result_df) <- paste0(c("z", "f"), score_name)
  result_df
}

#' standardise lenhei
#' if child is <= 730 days, lenhei_unit var should be 'L'.
#' If lenhei_unit var is 'H', must add 0.7cm to standardise
#' if child is > 730 days, lenhei_unit var should be 'H'.
#' If lenhei_unit var is 'L', must subtract 0.7cm to standardise
#' @noRd
adjust_lenhei <- function(age_in_days, measure, lenhei) {
  stopifnot(is.character(measure), is.numeric(age_in_days), is.numeric(lenhei))
  lenhei <-
    ifelse(
      !is.na(age_in_days) &
        age_in_days < 731 & !is.na(measure) & measure ==  "h",
      lenhei + 0.7,
      lenhei
    )

  lenhei <-
    ifelse(
      !is.na(age_in_days) &
        age_in_days >= 731 & !is.na(measure) & measure ==  "l",
      lenhei - 0.7,
      lenhei
    )
  lenhei
}

anthro_zscore_adjusted <-
function(name, measure, age_in_days, sex, growthstandards, flag_threshold,
         allowed_age_range = c(0, 1856),
         zscore_is_valid = rep.int(TRUE, length(measure)),
         zscore_fun = compute_zscore_adjusted) {

  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  stopifnot(is.numeric(measure))
  stopifnot(is.numeric(allowed_age_range), length(allowed_age_range) == 2L,
            !any(is.na(allowed_age_range)))
  stopifnot(is.logical(zscore_is_valid),
            length(zscore_is_valid) == length(measure))
  stopifnot(is.function(zscore_fun))
  assert_valid_sex(sex)
  age_in_days <- assert_valid_age_in_days(age_in_days)
  assert_growthstandards(growthstandards)

  # for all indicators a measure <= 0 should result in zscores being NA
  measure[measure <= 0] <- NA_real_

  # we convert the input parameter to a data frame and
  # join that with the growthstandards
  # then we have everything to compute the zscores
  zscore <- apply_zscore_and_growthstandards(zscore_fun, growthstandards,
                                             age_in_days, sex, measure)

  # we only compute zscores for children age <= 60 months
  age_in_months <- age_to_months(age_in_days, is_age_in_month = FALSE)
  valid_age <- age_in_months <= 60

  # at last we set certain zscores to NA
  valid_zscore <- !is.na(age_in_days) &
    age_in_days >= allowed_age_range[1L] &
    age_in_days <= allowed_age_range[2L] &
    zscore_is_valid &
    valid_age
  flag_zscore(flag_threshold, name, zscore, valid_zscore)
}