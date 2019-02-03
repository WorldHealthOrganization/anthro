#' Weight-for-length zscore indicator
#'
#' @param weight numeric
#' @param length_measure numeric
#' @param length_unit character
#' @param age_in_days integer, the age in days.
#' @param sex integer, the sex where 1 is male and 2 is female
#' @param flag_threshold numeric, a length 1 threshold.
#'        If the absolute value of the z-score is greater than
#'        this parameter, the z-score gets flagged in the resulting data frame.
#' @param growthstandards data.frame, the growstandards
#' table for the weight-for-age indicator.
#' Do not change unless you know what you are doing.
#' @include z-score-helper.R
#' @noRd
anthro_zscore_weight_for_lenhei <-
function(weight, lenhei, lenhei_unit, age_in_days, sex, oedema,
         flag_threshold = 5, growthstandards_wfl = growthstandards_wflanthro,
         growthstandards_wfh = growthstandards_wfhanthro) {

  stopifnot(is.numeric(weight))
  stopifnot(is.numeric(weight))
  stopifnot(is.character(oedema) && all(oedema %in% c("y", "n")))
  assert_valid_sex(sex)
  age_in_days <- assert_valid_age_in_days(age_in_days)
  assert_growthstandards(growthstandards_wfl)
  assert_growthstandards(growthstandards_wfh)

  # we convert the input parameter to a data frame and
  # join that with the growthstandards
  # then we have everything to compute the zscores
  n <- length(lenhei)

  # clean weight/lenhei
  weight[weight < 0.9 | weight > 58.0] <- NA_real_
  lenhei[lenhei < 38.0 | lenhei > 150.0] <- NA_real_

  # we also need to interpolate lenhei under certain coniditions
  low_lenhei <- trunc(lenhei * 10) / 10
  upp_lenhei <- trunc(lenhei * 10 + 1) / 10
  diff_lenhei <- (lenhei - low_lenhei) / 0.1

  # harmonize growthstandards, so we can join for both units
  colnames(growthstandards_wfl) <- c("sex", "lenhei", "l", "m", "s", "lorh")
  colnames(growthstandards_wfh) <- c("sex", "lenhei", "l", "m", "s", "lorh")
  growthstandards <- rbind(growthstandards_wfl, growthstandards_wfh)
  growthstandards[["lorh"]] <- tolower(growthstandards[["lorh"]])

  join_on_l <- ( (!is.na(age_in_days) & age_in_days < 731) |
                  (is.na(age_in_days) &
                     !is.na(lenhei_unit) &
                     lenhei_unit == "l") |
                  (is.na(age_in_days) &
                     is.na(lenhei_unit) &
                     !is.na(lenhei) & lenhei < 87))

  join_on_h <- ( (!is.na(age_in_days) & age_in_days >= 731) |
                 (is.na(age_in_days) &
                    !is.na(lenhei_unit) &
                    lenhei_unit == "h") |
                 (is.na(age_in_days) & is.na(lenhei_unit) &
                    !is.na(lenhei) & lenhei >= 87))

  input_df <- data.frame(weight, sex, lenhei_unit, low_lenhei, upp_lenhei,
                         diff_lenhei, ordering = seq_len(n),
                         join_col = ifelse(join_on_l, rep.int("l", n),
                                           ifelse(join_on_h,
                                                  rep.int("h", n),
                                                  rep.int(NA_character_, n))))
  merged_df <- merge(input_df,
                     growthstandards,
                     by.x = c("sex", "low_lenhei", "join_col"),
                     by.y = c("sex", "lenhei", "lorh"),
                     all.x = TRUE, sort = FALSE, suffixes = c("", "_lower"))
  merged_df <- merge(merged_df,
                     growthstandards,
                     by.x = c("sex", "upp_lenhei", "join_col"),
                     by.y = c("sex", "lenhei", "lorh"),
                     all.x = TRUE, sort = FALSE, suffixes = c("", "_upper"))
  merged_df <- merged_df[order(merged_df$ordering), ]

  y <- merged_df[["weight"]]
  m <- ifelse(diff_lenhei > 0,
              merged_df[["m"]] + diff_lenhei *
                (merged_df[["m_upper"]] - merged_df[["m"]]),
              merged_df[["m"]])
  m <- as.numeric(m)
  l <- ifelse(diff_lenhei > 0,
              merged_df[["l"]] + diff_lenhei *
                (merged_df[["l_upper"]] - merged_df[["l"]]),
              merged_df[["l"]])
  l <- as.numeric(l)
  s <- ifelse(diff_lenhei > 0,
              merged_df[["s"]] + diff_lenhei *
                (merged_df[["s_upper"]] - merged_df[["s"]]),
              merged_df[["s"]])
  s <- as.numeric(s)

  zscore <- compute_zscore_adjusted(y, m, l, s)
  zscore <- round(zscore, digits = 2L)

  valid_zscore <- !is.na(lenhei) &
                          ifelse(join_on_l,
                                 lenhei >= 45 & lenhei <= 110,
                                 ifelse(join_on_h,
                                        lenhei >= 65 & lenhei <= 120,
                                        rep.int(FALSE, n)))

  valid_zscore <- valid_zscore & !(oedema %in% "y")
  valid_zscore <- valid_zscore & (is.na(age_in_days) | (age_in_days <= 1856))

  flag_zscore(flag_threshold, "wfl", zscore, valid_zscore)
}
