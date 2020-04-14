# cleans the sex variable
standardize_sex_var <- function(sex) {
  sex <- tolower(trimws(as.character(sex)))
  sex <- ifelse(!is.na(sex) & (sex == "m" | sex == "1"), 1L,
    ifelse(!is.na(sex) & (sex == "f" | sex == "2"),
      2L, NA_integer_
    )
  )
  sex
}

standardize_oedema_var <- function(oedema) {
  oedema <- tolower(trimws(oedema))
  oedema[oedema %in% "1"] <- "y"
  oedema[oedema %in% "2"] <- "n"
  oedema[!(oedema %in% "y")] <- "n"
  oedema
}

age_to_days <- function(age, is_age_in_month) {
  if (is_age_in_month) {
    age * ANTHRO_DAYS_OF_MONTH
  } else {
    age
  }
}

age_under_60_month <- function(age_in_days) {
  age_in_months <- age_to_months(age_in_days, is_age_in_month = FALSE)
  age_in_months < 60
}

#' banker's rounding for 0 digits and positive numerics
#' i.e. < .5 down, >= .5 up
#' @noRd
round_up <- function(x) {
  stopifnot(is.numeric(x), all(x >= 0, na.rm = TRUE))
  if (length(x) == 0) {
    return(numeric())
  }
  x_rounded <- floor(x)
  rest <- x - x_rounded
  rounded_up <- rest >= 0.5
  rounded_up[is.na(rounded_up)] <- FALSE
  x_rounded[rounded_up] <- x_rounded[rounded_up] + 1
  x_rounded
}

age_to_months <- function(age, is_age_in_month) {
  if (is_age_in_month) {
    age
  } else {
    age / ANTHRO_DAYS_OF_MONTH
  }
}

#' Groups age in months into groups
#' @param age_in_months numeric vector of positive ages
#' @noRd
anthro_age_groups <- function(age_in_months) {
  stopifnot(is.numeric(age_in_months))
  cut_breaks <- c(0, 6, 12, 24, 36, 48, 60)
  cut_labels <- c(
    "00-05 mo",
    "06-11 mo",
    "12-23 mo",
    "24-35 mo",
    "36-47 mo",
    "48-59 mo"
  )
  cut(age_in_months,
    breaks = cut_breaks,
    labels = cut_labels,
    right = FALSE
  )
}
