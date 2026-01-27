#' @export
column_values.survey_design <- function(x, col) {
  x$design$variables[[col]]
}

#' @export
column_names.survey_design <- function(x) {
  colnames(x$design$variables)
}

#' @export
compute_prevalence_zscore_summaries_by.survey_design <- function(
  data,
  indicator,
  subset_col_name
) {
  zscore_col_name <- prev_zscore_value_column(indicator)
  zscore_formula <- as.formula(paste0("~", zscore_col_name))
  subset_formula <- as.formula(paste0("~", subset_col_name))
  mean_est_summary <- svyby(
    zscore_formula,
    subset_formula,
    data$design,
    svymean,
    na.rm = TRUE,
    na.rm.all = TRUE,
    drop.empty.groups = FALSE
  )
  mean_est_ci_summary <- confint(
    mean_est_summary,
    df = degf(data$design),
    level = 1 - prevalence_significance_level
  )

  # when there is only one observation in the subset, svyvar returns a
  # length 2 object which breaks svyby. We thus provide a robust
  # version of svyvar that returns NA if a length 2 object is returned.
  # this will be fixed in survey >= 4.2
  robust_svyvar <- function(x, design, na.rm = FALSE, ...) {
    res <- svyvar(x, design, na.rm = na.rm, ...)
    if (length(res) == 2) {
      new_res <- NA_real_
      attr(new_res, "names") <- prev_zscore_value_column(indicator)
      attr(new_res, "var") <- NA_real_
      attr(new_res, "statistic") <- "variance"
      class(new_res) <- c("svyvar", "svystat", "numeric")
      return(new_res)
    }
    res
  }
  mean_est_sd_summary <- svyby(
    zscore_formula,
    subset_formula,
    data$design,
    robust_svyvar,
    na.rm = TRUE,
    na.rm.all = TRUE,
    drop.empty.groups = FALSE
  )
  data.frame(
    Group = as.character(mean_est_summary[[subset_col_name]]),
    r = mean_est_summary[[prev_zscore_value_column(indicator)]],
    se = survey::SE(mean_est_summary),
    ll = mean_est_ci_summary[, 1L, drop = TRUE],
    ul = mean_est_ci_summary[, 2L, drop = TRUE],
    stdev = sqrt(mean_est_sd_summary[, 2L, drop = TRUE]),
    stringsAsFactors = FALSE
  )
}

#' @export
compute_prevalence_sample_size_by.survey_design <- function(
  data,
  indicator,
  subset_col_name
) {
  expr_name <- paste0("I(!is.na(", prev_prevalence_column_name(indicator), "))")
  prev_formula <- as.formula(paste0("~", expr_name))
  subset_formula <- as.formula(paste0("~", subset_col_name))
  prev <- svyby(
    prev_formula,
    subset_formula,
    data$design,
    svytotal,
    drop.empty.groups = FALSE
  )
  unweighted_prev <- svyby(
    prev_formula,
    subset_formula,
    data$design_unweighted,
    svytotal,
    drop.empty.groups = FALSE
  )
  pop_weighted <- prev[[paste0(expr_name, "TRUE")]]
  pop_unweighted <- unweighted_prev[[paste0(expr_name, "TRUE")]]

  # syby returns NA for empty levels. We set the count to 0 for these.
  pop_weighted[is.na(pop_weighted)] <- 0
  pop_unweighted[is.na(pop_unweighted)] <- 0

  stopifnot(
    # check that subsets come in the right order
    all(prev[[subset_col_name]] == unweighted_prev[[subset_col_name]])
  )
  data.frame(
    Group = as.character(prev[[subset_col_name]]),
    pop = pop_weighted,
    unwpop = pop_unweighted,
    stringsAsFactors = FALSE
  )
}

#' @export
compute_prevalence_estimates_for_column_by.survey_design <- function(
  data,
  indicator_name,
  subset_col_name,
  prev_col_name
) {
  subset_formula <- as.formula(paste0("~", subset_col_name))
  prev_col_formula <- as.formula(paste0("~", prev_col_name))
  mean_est_prev <- svyby(
    prev_col_formula,
    subset_formula,
    data$design,
    svymean,
    na.rm = TRUE,
    na.rm.all = TRUE,
    drop.empty.groups = FALSE
  )
  mean_est_ci_prev <- svyby(
    prev_col_formula,
    subset_formula,
    data$design,
    svyciprop,
    vartype = "ci",
    df = degf(data$design),
    method = "logit",
    drop.empty.groups = FALSE,
    na.rm.all = TRUE,
    level = 1 - prevalence_significance_level
  )[, 3L:4L]
  res <- data.frame(
    Group = as.character(mean_est_prev[[subset_col_name]]),
    r = mean_est_prev[[prev_col_name]] * 100,
    se = survey::SE(mean_est_prev) * 100,
    ll = mean_est_ci_prev$ci_l * 100,
    ul = mean_est_ci_prev$ci_u * 100,
    stringsAsFactors = FALSE
  )
  # For the extreme cases of `r = 0` and `r = 1` we set the CIs
  # to [0,0] and [1,1] respectively. Mostly for the convenience
  # of the human user who consumes the prevalence estimates and to be
  # in line with the method of the `simple` computation.
  boundary_0 <- res$r == 0
  boundary_1 <- res$r == 1
  res$ll[boundary_0] <- 0
  res$ul[boundary_0] <- 0
  res$ll[boundary_1] <- 1
  res$ul[boundary_1] <- 1
  res
}
