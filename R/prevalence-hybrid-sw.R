#' The following is a hybrid backed for cases with a sampling weight > 0.
#' It recreates the estimates for z-score summaries and population estimates,
#' but uses the survey package to compute estimates around the 0/1 indicators.
#'
#' The z-score estimates follow the method uses in the {survey} package by
#' T. Lumley.
#'
#' Reference:
#' T. Lumley (2024) "survey: analysis of complex survey samples".
#' R package version 4.4.
#' @noRd

#' @export
column_names.hybrid_design <- function(x) {
  colnames(x$data)
}

#' @export
column_values.hybrid_design <- function(x, col) {
  x$data[[col]]
}

#' @export
#' @include prevalence-simple.R
compute_prevalence_zscore_summaries_by.hybrid_design <- function(
  data,
  indicator,
  subset_col_name
) {
  zscore_col_name <- prev_zscore_value_column(indicator)
  compute_and_aggregate(
    data,
    zscore_col_name,
    subset_col_name,
    compute = zscore_estimate_weighted,
    empty_data_prototype = data.frame(
      r = NA_real_,
      se = NA_real_,
      ll = NA_real_,
      ul = NA_real_,
      stdev = NA_real_
    )
  )
}

#' @export
compute_prevalence_estimates_for_column_by.hybrid_design <- compute_prevalence_estimates_for_column_by.survey_design

#' @export
#' @include prevalence-simple.R
compute_prevalence_sample_size_by.hybrid_design <- function(
  data,
  indicator,
  subset_col_name
) {
  column_name <- prev_prevalence_column_name(indicator)
  compute_and_aggregate(
    data,
    column_name,
    subset_col_name,
    compute = sample_size_weighted,
    empty_data_prototype = data.frame(
      pop = 0,
      unwpop = 0
    )
  )
}

#' @importFrom stats sd plogis qt
zscore_estimate_weighted <- function(x, N, weights, empty_data_prototype) {
  non_na <- !is.na(x)
  x <- x[non_na]
  if (length(x) == 0) {
    return(empty_data_prototype)
  }
  w <- weights[non_na]
  n <- length(x)
  sw <- sum(w)
  r <- sum(w * x) / sw
  w_normalized <- w / sw
  stdev <- sqrt(sum(w_normalized * (x - r)^2) * n / (n - 1))
  se <- sample_se_weights(x, w, r, sw, N)
  t_value <- qt(1 - prevalence_significance_level / 2, N - 1)
  cis <- r + se * c(-t_value, t_value)
  data.frame(
    r = r,
    se = se,
    ll = cis[1L],
    ul = cis[2L],
    stdev = stdev
  )
}

sample_size_weighted <- function(x, N, weights, empty_data_prototype) {
  x <- as.numeric(!is.na(x))
  pop <- as.numeric(sum(x * weights))
  n <- as.numeric(sum(x))
  data.frame(pop = pop, unwpop = n)
}

sample_se_weights <- function(x, weights, x_mean, n, N) {
  scale <- N / (N - 1)
  x_deviation <- weights * (x - x_mean)
  sqrt(scale) / n * sqrt(sum(x_deviation * x_deviation))
}
