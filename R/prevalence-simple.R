#' The following is a fast approximation of what the survey package
#' computes given cluster/strata/sw is NULL.
#'
#' T. Lumley (2024) "survey: analysis of complex survey samples".
#' R package version 4.4.
#'
#' The speedup and memory savings can be substantial in particular for very
#' large surveys with > 100.000 observations.
#'
#' The simple here refers to the absence of cluster/strata/sw, making the
#' structure a bit more simple.
#' @noRd

#' @export
column_names.simple_design <- function(x) {
  colnames(x$data)
}

#' @export
column_values.simple_design <- function(x, col) {
  x$data[[col]]
}

#' @export
compute_prevalence_zscore_summaries_by.simple_design <- function(
  data,
  indicator,
  subset_col_name
) {
  zscore_col_name <- prev_zscore_value_column(indicator)
  compute_and_aggregate(
    data,
    zscore_col_name,
    subset_col_name,
    compute = zscore_estimate,
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
compute_prevalence_estimates_for_column_by.simple_design <- function(
  data,
  indicator_name,
  subset_col_name,
  prev_col_name
) {
  compute_and_aggregate(
    data,
    prev_col_name,
    subset_col_name,
    compute = logit_rate_estimate,
    empty_data_prototype = data.frame(
      r = NA_real_,
      se = NA_real_,
      ll = NA_real_,
      ul = NA_real_
    )
  )
}

#' @export
compute_prevalence_sample_size_by.simple_design <- function(
  data,
  indicator,
  subset_col_name
) {
  column_name <- prev_prevalence_column_name(indicator)
  compute_and_aggregate(
    data,
    column_name,
    subset_col_name,
    compute = sample_size,
    empty_data_prototype = data.frame(
      pop = 0,
      unwpop = 0
    )
  )
}

#' @importFrom stats aggregate
compute_and_aggregate <- function(
  data,
  value_column,
  subset_column,
  compute,
  empty_data_prototype
) {
  col_values <- column_values(data, value_column)
  N <- length(col_values)
  weights <- data$data[["sampling_weights"]]
  has_weights <- !is.null(weights)
  grouping <- column_values(data, subset_column)
  groups <- if (is.factor(grouping)) {
    levels(grouping)
  } else {
    unique(grouping)
  }
  grouping <- as.character(grouping)
  values <- vector(mode = "list", length(groups))
  names(values) <- groups
  for (group in groups) {
    values[[group]] <- if (has_weights) {
      subroup <- grouping == group
      compute(
        col_values[subroup],
        N = N,
        weights = weights[subroup],
        empty_data_prototype = empty_data_prototype
      )
    } else {
      compute(
        col_values[grouping == group],
        N = N,
        empty_data_prototype = empty_data_prototype
      )
    }
  }
  Group <- names(values)
  data <- do.call(rbind, values)
  data <- cbind(Group, data)
  rownames(data) <- NULL
  data
}

#' @importFrom stats plogis qt
logit_rate_estimate <- function(x, N, empty_data_prototype) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(empty_data_prototype)
  }
  n <- length(x)
  r <- mean(x == 1)
  se <- sample_se(x, r, n, N)

  cis <- if (r == 1) {
    c(1, 1)
  } else if (r == 0) {
    c(0, 0)
  } else {
    logit_r <- log(r / (1 - r))
    logit_r_var <- 1 / (n * r * (1 - r))
    scale <- N / (N - 1)
    se_logit <- sqrt(scale * logit_r_var)
    t_value <- qt(1 - prevalence_significance_level / 2, df = N - 1)
    cis <- plogis(
      logit_r + se_logit * c(-t_value, t_value)
    )
    as.numeric(cis)
  }

  data.frame(
    r = r * 100,
    se = se * 100,
    ll = cis[1L] * 100,
    ul = cis[2L] * 100
  )
}

#' @importFrom stats sd plogis qt
zscore_estimate <- function(x, N, empty_data_prototype) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(empty_data_prototype)
  }
  n <- length(x)
  r <- mean(x)
  stdev <- sd(x)
  se <- sample_se(x, r, n, N)
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

sample_size <- function(x, N, empty_data_prototype) {
  n <- as.numeric(sum(!is.na(x)))
  data.frame(pop = n, unwpop = n)
}

sample_se <- function(x, x_mean, n, N) {
  scale <- N / (N - 1)
  x_deviation <- x - x_mean
  sqrt(scale) / n * sqrt(sum(x_deviation * x_deviation))
}
