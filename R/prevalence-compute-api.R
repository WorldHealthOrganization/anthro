#' In order to support different types of computation for special cases of the
#' data we define here a compute API that different backends can hook into.
#' @noRd
NULL

#' Computes the sample size by indicator and subset
#' @noRd
compute_prevalence_sample_size_by <- function(
  data,
  indicator,
  subset_col_name
) {
  UseMethod("compute_prevalence_sample_size_by")
}

#' Computes prevalence of rates for a given indicator, subset and cutoff
#' @noRd
compute_prevalence_estimates_for_column_by <- function(
  data,
  indicator_name,
  subset_col_name,
  prev_col_name
) {
  UseMethod("compute_prevalence_estimates_for_column_by")
}

#' Computes prevalence of zscores by indicator and subset
#' @noRd
compute_prevalence_zscore_summaries_by <- function(
  data,
  indicator,
  subset_col_name
) {
  UseMethod("compute_prevalence_zscore_summaries_by")
}

#' Returns the values for a specific column
#' @noRd
column_values <- function(x, col) {
  UseMethod("column_values")
}

#' Returns the column names of the data
#' @noRd
column_names <- function(x) {
  UseMethod("column_names")
}
