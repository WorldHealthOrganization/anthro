#' Compute prevalence estimates
#'
#' Prevalence estimates according to the WHO recommended standard analysis:
#' includes prevalence estimates with corresponding standard errors
#' and confidence intervals, and z-score summary statistics
#' (mean and standard deviation) with most common cut-offs describing the
#' full index distribution (-3, -2, -1, +1, +2, +3), and at disaggregated
#' levels for all available factors (age, sex, type of residence,
#' geographical regions, wealth quintiles, mother education and one
#' additional factor the user is interested in and for
#' which data are available).
#'
#' In this function, all available (non-missing and non-flagged) z-score values
#' are used for each indicator-specific prevalence
#' estimation (standard analysis).
#'
#' Note: the function temporarily sets the \code{survey} option
#' \code{survey.lonely.psu} to "adjust" and then restores the original values.
#' The function is a wrapper around the \code{survey} package to compute
#' estimates for the different groups (e.g. by age or sex).
#'
#' If not all parameter values have equal length, parameter values will be
#' repeated to match the maximum length of all arguments except
#' \code{is_age_in_month} using \code{rep_len}. This happens without warnings.
#'
#' @inheritParams anthro_zscores
#'
#' @param sw An optional numeric vector containing the sampling weights.
#' If NULL, no sampling weights are used.
#'
#' @param cluster An optional integer vector representing clusters. If the value
#' is NULL this is treated as a survey without a cluster. This is also the case
#' if all values are equal, then we assume there is also no cluster.
#' @param strata An optional integer vector representing strata. Pass NULL to
#' indicate that there are no strata.
#'
#' @param typeres An optional integer or character vector representing a type of
#' residence.
#' Any values are accepted, however, “Rural” or “Urban” are preferable
#' for outputs purposes.
#'
#' @param gregion An optional integer or character vector representing a
#' geographical region.
#'
#' @param wealthq An optional integer or character vector representing wealth
#' quintiles where (1=poorest; 2,3,4,5=richest).
#' All values can either be NA, or 1, 2, 3, 4, 5 or Q1, Q2, Q3, Q4, Q5.
#'
#' @param mothered An optional integer or character vector representing the
#' education of the mother.
#' Any number of categories is accepted for the analysis, provided
#' sample sizes are sufficient in all categories. However, the common,
#' standard recommended categories are no education, primary school,
#' and secondary school or higher (“None”, “Primary” and “Secondary”).
#' Note: Mother education refers to the highest level of schooling
#' attained by the mother
#'
#' @param othergr An optional integer or character vector that is of interest
#' for stratified analysis.
#'
#' @examples
#' \dontrun{
#' # because it takes too long for CRAN checks
#' library(anthro)
#'
#' # compute the prevalence estimates for 100 random children
#' # with weight around 15kg and height around 100cm
#' res <- anthro_prevalence(
#'   sex = c(1, 2),
#'   age = 1000, # days
#'   weight = rnorm(100, 15, 1),
#'   lenhei = rnorm(100, 100, 10)
#' )
#'
#' # Height-for-age
#' # We extract prevalence estimates for <-3SD, <-2SD (Stunting)
#' # and the z-score mean
#' col_names <- c("Group", "HAZ_unwpop", "HA_3_r", "HA_2_r", "HA_r")
#' res <- res[, col_names]
#'
#' # rename the columns
#' colnames(res) <- c("Group", "Unweighted N", "-3SD", "-2SD", "z-score mean ")
#'
#' # note that we only generated data for one age group
#' res
#' }
#' @return Returns a data.frame with prevalence estimates for the various
#' groups.
#'
#' The output data frame includes prevalence estimates with corresponding
#' standard errors and confidence intervals,
#' and z-score summary statistics (mean and standard deviation) with most
#' common cut-offs describing the full index
#' distribution (-3, -2, -1, +1, +2, +3), and at disaggregated levels for
#' all available factors (age, sex, type of residence,
#' geographical regions, wealth quintiles, mother education and one
#' additional factor the user is interested in and for which
#' data are available).
#'
#' The resulting columns are coded with a \emph{prefix},
#' \emph{a prevalence indicator} and \emph{a suffix}:
#'
#' \strong{Prefix:}
#' \describe{
#'  \item{HA}{Height-for-age}
#'  \item{WA}{Weight-for-age}
#'  \item{WA_2}{Underweight}
#'  \item{BMI}{Body-mass-index-for-age}
#'  \item{WH}{Weight-for-height}
#'  \item{HA_WH}{Height-for-age and weight-for-height combined}
#' }
#'
#' \strong{Prevalence indicator:}
#' \describe{
#'  \item{_3}{Prevalence corresponding to < -3 SD}
#'  \item{_2}{Prevalence corresponding to < -2 SD}
#'  \item{_1}{Prevalence corresponding to < -1 SD}
#'  \item{1}{Prevalence corresponding to > +1 SD}
#'  \item{2}{Prevalence corresponding to > +2 SD}
#'  \item{3}{Prevalence corresponding to > +3 SD}
#' }
#'
#' \strong{Suffix:}
#' \describe{
#'  \item{_pop}{Weighted sample size}
#'  \item{_unwpop}{Unweighted sample size}
#'  \item{_r}{Mean/prevalence}
#'  \item{_ll}{lower 95\% confidence interval limit}
#'  \item{_ul}{upper 95\% confidence interval limit}
#'  \item{_stdev}{Standard Deviation}
#'  \item{_se}{Standard error}
#' }
#'
#'
#' \strong{For example:}
#' \describe{
#'   \item{WHZ_pop}{Weight-for-height weighted sample size}
#'   \item{HA_r}{Height-for-age z-score mean}
#'   \item{WA_stdev}{Weight-for-age z-score Standard Deviation}
#'   \item{WH2_r}{Prevalence of weight-for-height >+2 SD (overweight )}
#'   \item{WH_r}{Mean weight-for-height z-score}
#'   \item{BMI_2_se}{Prevalence of BMI-for-age <-2 SD standard error}
#'   \item{BMI_3_ll}{Prevalence of BMI-for-age <-3 SD lower 95\% confidence
#'   interval limit}
#'   \item{HA_2_WH_2_ul}{Prevalence of children Height-for-age and
#'   weight-for-height combined (stunted & wasted) lower 95\% confidence
#'   interval limit}
#' }
#'
#' @include anthro-package.R
#' @include assertions.R
#' @importFrom stats confint
#' @importFrom stats as.formula
#' @importFrom stats setNames
#' @export
anthro_prevalence <- function(sex,
                              age = NA_real_,
                              is_age_in_month = FALSE,
                              weight = NA_real_,
                              lenhei = NA_real_,
                              measure = NA_character_,
                              oedema = "n",
                              sw = NULL,
                              cluster = NULL,
                              strata = NULL,
                              typeres = NA_character_,
                              gregion = NA_character_,
                              wealthq = NA_character_,
                              mothered = NA_character_,
                              othergr = NA_character_) {
  # the other variables are being checked by anthro_zscores
  assert_character_or_numeric(typeres)
  assert_character_or_numeric(gregion)
  assert_character_or_numeric(wealthq)
  assert_character_or_numeric(mothered)
  assert_character_or_numeric(othergr)
  is.null(cluster) || assert_numeric(cluster)
  is.null(strata) || assert_numeric(strata)
  is.null(sw) || assert_numeric(sw)

  # make all input lengths equal by constructing a data.frame
  input <- data.frame(
    sex,
    age,
    weight,
    lenhei,
    measure,
    oedema,
    typeres,
    gregion,
    wealthq,
    mothered,
    othergr
  )
  if (!is.null(cluster)) {
    input[["cluster"]] <- cluster
  }
  if (!is.null(strata)) {
    input[["strata"]] <- strata
  }
  if (!is.null(sw)) {
    input[["sw"]] <- sw
  }

  # zscore data
  zscores <- anthro_zscores(
    sex = input[["sex"]],
    age = input[["age"]],
    is_age_in_month = is_age_in_month,
    weight = input[["weight"]],
    lenhei = input[["lenhei"]],
    measure = input[["measure"]],
    oedema = input[["oedema"]]
  )
  zscores_orig <- zscores
  stopifnot(c("zlen", "zwei", "zwfl", "zbmi") %in% colnames(zscores))

  input[["age_in_days"]] <- age_to_days(input[["age"]], is_age_in_month = is_age_in_month)
  input[["age_in_months"]] <- age_to_months(input[["age"]], is_age_in_month = is_age_in_month)

  input[["oedema"]] <- standardize_oedema_var(input[["oedema"]])

  # we compute the age group with full precision
  input[["age_group"]] <- anthro_age_groups(input[["age_in_months"]])

  # Make Oedema variable to be "n" if age > than 1826 completed days
  # (children w/ oedema count in prevalence even if z-score missing
  # for weight related indicators)
  # this is redundant with the filter that age_in_days <= 1826
  stopifnot(is.character(input[["oedema"]]), all(!is.na(input[["oedema"]]) &
    oedema %in% c("n", "y")))
  oedema_to_n <- !is.na(input[["age_in_days"]]) & input[["age_in_days"]] > 1826
  input[["oedema"]][oedema_to_n] <- "n"

  # retain only those records where age <= 1826 or is.na(age)
  filter_zscores <- is.na(input[["age_in_days"]]) | input[["age_in_days"]] <= 1826

  # warn if rows are excluded
  no_excluded <- sum(!filter_zscores, na.rm = TRUE)
  if (no_excluded > 0L) {
    row_label <- if (no_excluded == 1L) "row" else "rows"
    warning(no_excluded, " ", row_label,
      " will be excluded for the prevalence ",
      "computation due to missing age or age > 1826 days ",
      call. = FALSE
    )
  }

  zscores <- zscores[filter_zscores, , drop = FALSE]
  input <- input[filter_zscores, , drop = FALSE]

  valid_wealthq_values <- is.na(input[["wealthq"]]) |
    input[["wealthq"]] %in% as.character(1L:5L) |
    input[["wealthq"]] %in% paste0("Q", 1L:5L)
  if (!all(valid_wealthq_values)) {
    warning("Some entries in wealthq are out of range. Allowed values are: ",
      "NA, 1, 2, 3, 4, 5 or Q1, Q2, Q3, Q4, Q5. ",
      "All other values will be converted to NA",
      call. = FALSE
    )
    input[["wealthq"]][!valid_wealthq_values] <- NA_character_
  }
  wealth_q_in <- function(x) {
    input[["wealthq"]] %in% c(x, paste0("Q", x))
  }
  input[["wealthq"]][wealth_q_in("1")] <- "Q1: Poorest"
  input[["wealthq"]][wealth_q_in("2")] <- "Q2"
  input[["wealthq"]][wealth_q_in("3")] <- "Q3"
  input[["wealthq"]][wealth_q_in("4")] <- "Q4"
  input[["wealthq"]][wealth_q_in("5")] <- "Q5: Richest"
  input[["wealthq"]] <- factor(input[["wealthq"]], levels = c(
    "Q1: Poorest", "Q2", "Q3",
    "Q4", "Q5: Richest"
  ))

  input[["sex"]] <- as.character(standardize_sex_var(input[["sex"]]))
  input[["sex"]][input[["sex"]] %in% "1"] <- "Male"
  input[["sex"]][input[["sex"]] %in% "2"] <- "Female"
  input[["sex"]] <- factor(input[["sex"]], levels = c("Female", "Male"))

  input[["age_sex_interaction"]] <- interaction(input[["age_group"]], input[["sex"]])

  # here we prepare all different strata that are being computed
  optional_strata_labels <-
    c(
      "Area",
      "Geographical region",
      "Wealth quintile",
      "Maternal education",
      "Other grouping"
    )

  strata_label <-
    c("All", "Age group", "Sex", "Age + sex", optional_strata_labels)
  strata_values <- list(
    rep.int("All", nrow(input)), # a strata for total
    input[["age_group"]],
    input[["sex"]],
    input[["age_sex_interaction"]],
    input[["typeres"]],
    input[["gregion"]],
    input[["wealthq"]],
    input[["mothered"]],
    input[["othergr"]]
  )

  included_strata <- vapply(
    strata_values,
    function(x) !all(is.na(x)),
    logical(1L)
  )
  if (all(included_strata == FALSE)) {
    stop("It seems that all values have been removed from the analysis, ",
      "due to specific exclusion rules. We stop the calculation here.",
      call. = FALSE
    )
  }
  strata_values <- strata_values[included_strata]
  strata_label <- strata_label[included_strata]
  strata_cols <- list(
    "all", # a strata for total
    "age_group",
    "sex",
    "age_sex_interaction",
    "typeres",
    "gregion",
    "wealthq",
    "mothered",
    "othergr"
  )
  prev_data <- cbind(
    zscores_orig[filter_zscores, , drop = FALSE],
    data.frame(
      all = "All",
      age_group = input[["age_group"]],
      sex = input[["sex"]],
      age_sex_interaction = input[["age_sex_interaction"]],
      typeres = input[["typeres"]],
      gregion = input[["gregion"]],
      wealthq = input[["wealthq"]],
      mothered = input[["mothered"]],
      othergr = input[["othergr"]],
      oedema = input[["oedema"]]
    )
  )
  if (!is.null(input[["cluster"]])) {
    prev_data[["cluster"]] <- input[["cluster"]]
  }
  if (!is.null(input[["strata"]])) {
    prev_data[["strata"]] <- input[["strata"]]
  }
  if (!is.null(input[["sw"]])) {
    prev_data[["sampling_weights"]] <- input[["sw"]]
  }

  # Before we compute all prevalence indicators we temporarily set a
  # survey option and restore it on exit
  opts <- options(survey.lonely.psu = "adjust")
  on.exit(options(opts))

  compute_prevalence_of_zscores(
    data = prev_data,
    zscores_to_compute = list(
      list(
        name = "HA", column = "len",
        with_cutoffs = TRUE, with_auxiliary_zscore_column = FALSE
      ),
      list(
        name = "WA", column = "wei",
        with_cutoffs = TRUE, with_auxiliary_zscore_column = TRUE
      ),
      list(
        name = "BMI", column = "bmi",
        with_cutoffs = TRUE, with_auxiliary_zscore_column = TRUE
      ),
      list(
        name = "WH", column = "wfl",
        with_cutoffs = TRUE, with_auxiliary_zscore_column = TRUE
      )
    ),
    survey_subsets = setNames(strata_cols[included_strata], strata_label)
  )
}


#' @importFrom survey svytotal
#' @importFrom survey svyby
#' @importFrom survey svyciprop
#' @importFrom survey degf
#' @importFrom survey svymean
#' @importFrom survey svyvar
compute_prevalence_of_zscores <- function(data,
                                          zscores_to_compute,
                                          survey_subsets) {
  stopifnot(
    is.data.frame(data),
    all(c("oedema") %in% colnames(data)),
    is.list(zscores_to_compute),
    all(vapply(zscores_to_compute, function(x) {
      is.list(x)
    }, logical(1L))),
    is.null(data[["sampling_weights"]]) || is.numeric(data[["sampling_weights"]]),
    is.list(survey_subsets),
    is.character(names(survey_subsets))
  )

  if (!is.null(data[["sampling_weights"]]) && any(data[["sampling_weights"]] < 0, na.rm = TRUE)) {
    stop(
      "Negative sampling weights are not allowed.",
      call. = FALSE
    )
  }

  zscores_to_compute <- lapply(zscores_to_compute, function(indicator) {
    if (isTRUE(indicator$with_cutoffs)) {
      indicator$cutoffs <- generate_cutoffs(prev_prevalence_column_name(indicator))
    }
    indicator
  })

  # exclude rows with cluster/strata NA if not all are NA
  old_data_nrows <- nrow(data)
  has_cluster_info <- !is.null(data[["cluster"]])
  has_strata_info <- !is.null(data[["strata"]])
  if (has_cluster_info) {
    data <- data[!is.na(data[["cluster"]]), , drop = FALSE]
  }
  if (has_strata_info) {
    data <- data[!is.na(data[["strata"]]), , drop = FALSE]
  }

  # do some other warnings/stops that a applicable broadly
  # warn if rows are excluded
  no_excluded <- old_data_nrows - nrow(data)
  if (no_excluded > 0L) {
    row_label <- if (no_excluded == 1L) "row" else "rows"
    warning(no_excluded, " ", row_label,
      " will be excluded for the prevalence ",
      "computation due to missing cluster and strata.",
      call. = FALSE
    )
  }

  # set all sw that are NA to 0
  if (!is.null(data[["sampling_weights"]]) &&
    anyNA(data[["sampling_weights"]])) {
    sw <- data[["sampling_weights"]]
    na_sw <- is.na(sw)
    sw[na_sw] <- 0
    data[["sampling_weights"]] <- sw
    no_excluded <- sum(na_sw)
    row_label <- if (no_excluded == 1L) "row" else "rows"
    warning(no_excluded, " ", row_label, " had missing sampling weights",
      " and they will be set to 0. ",
      "Doing this excludes them in the prevalence calculation.",
      call. = FALSE
    )
  }

  res <- set_flagged_zscores_to_NA(zscores_to_compute, data)
  res <- create_zscore_auxiliary_columns(zscores_to_compute, res)
  res <- create_cutoff_columns_for_each_zscore(zscores_to_compute, res)
  survey_design <- build_survey_design(res)

  final_result <- compute_prevalence_for_each_subset_and_indicator(
    subset_col_names = survey_subsets,
    subset_labels = names(survey_subsets),
    survey_design = survey_design,
    indicators = zscores_to_compute
  )
  final_result
}

generate_cutoffs <- function(data_column) {
  lapply(c(-3, -2, -1, 1, 2, 3), function(cutoff) {
    cutoff_fun <- if (cutoff < 0) {
      function(x) ifelse(x < cutoff, 1L, 0L)
    } else {
      function(x) ifelse(x > cutoff, 1L, 0L)
    }
    list(
      suffix = paste0(if (cutoff < 0) "_", abs(cutoff)),
      fun = cutoff_fun,
      data_column = data_column
    )
  })
}

#' @importFrom survey svydesign
build_survey_design <- function(survey_data) {
  has_cluster_info <- !is.null(survey_data[["cluster"]])
  has_strata_info <- !is.null(survey_data[["strata"]])
  has_sampling_weights <- !is.null(survey_data[["sampling_weights"]])
  just_one_cluster <- has_cluster_info && length(unique(survey_data[["cluster"]])) == 1L
  any_strata_na <- anyNA(survey_data[["strata"]])
  stopifnot(!any_strata_na)
  cluster_formula <- if (just_one_cluster || !has_cluster_info) {
    ~1
  } else {
    ~cluster
  }
  strata_formula <- if (has_strata_info) {
    ~strata
  }
  sampling_weights_formula <- if (has_sampling_weights) {
    ~sampling_weights
  }
  design <- svydesign(
    ids = cluster_formula,
    strata = strata_formula,
    weights = sampling_weights_formula,
    data = survey_data,
    nest = TRUE
  )
  design_unweighted <- svydesign(
    ids = cluster_formula,
    strata = strata_formula,
    weights = NULL,
    data = survey_data,
    nest = TRUE
  )
  list(
    design = design,
    design_unweighted = design_unweighted
  )
}

compute_prevalence_stunting_wasting <- function(survey_design, subset_col_name) {
  indicator <- list(
    name = "HA_2_WH_2",
    column = "len_zwfl_aux_stunting_wasting",
    cutoffs = list(),
    prevalence_column = "zlen_zwfl_aux_stunting_wasting"
  )
  list(
    compute_prevalence_sample_size(survey_design, indicator, subset_col_name),
    compute_prevalence_estimates_for_column(
      survey_design$design, indicator$name,
      subset_col_name, prev_prevalence_column_name(indicator)
    )
  )
}

compute_prevalence_stunting_overweight <- function(survey_design, subset_col_name) {
  indicator <- list(
    name = "HA_2_WH2",
    column = "len_zwfl_aux_stunting_overweight",
    cutoffs = list(),
    zscore_column = "zlen_zwfl_aux_stunting_overweight"
  )
  list(
    compute_prevalence_sample_size(survey_design, indicator, subset_col_name),
    compute_prevalence_estimates_for_column(
      survey_design$design, indicator$name,
      subset_col_name, prev_prevalence_column_name(indicator)
    )
  )
}

compute_prevalence_for_each_subset_and_indicator <- function(subset_col_names,
                                                             subset_labels,
                                                             survey_design,
                                                             indicators) {
  has_zlen_and_wfl <- all(
    c("zlen", "zwfl_aux") %in% colnames(survey_design$design$variables)
  )
  # This double apply could be moved down to the survey package as it
  # also supports calculating multiple values at once.
  subset_results <- mapply(function(subset_col_name, label) {
    indicator_results <- lapply(indicators, function(indicator) {
      # some svyby calls produce glm.fit warnings.
      # It was decided to suppress these warnings.
      suppressWarnings({
        c(
          list(compute_prevalence_sample_size(
            survey_design, indicator, subset_col_name
          )),
          compute_prevalence_cutoff_summaries(
            survey_design$design, indicator, subset_col_name
          ),
          list(compute_prevalence_zscore_summaries(
            survey_design$design, indicator, subset_col_name
          ))
        )
      })
    })
    indicator_results <- unlist(indicator_results, recursive = FALSE) # flatten

    suppressWarnings({
      if (has_zlen_and_wfl) {
        indicator_results <- c(
          indicator_results,
          compute_prevalence_stunting_wasting(survey_design, subset_col_name),
          compute_prevalence_stunting_overweight(survey_design, subset_col_name)
        )
      }
    })

    # now we merge everything into one df
    # we assume the first indicator results DF has all levels of the Group
    # then we merge everything and after that we have to restore the original
    # ordering of the levels (they get lost by the `merge` calls (sometimes))
    original_group_ordering <- indicator_results[[1]][["Group"]]
    res <- Reduce(function(acc, el) {
      merge(acc, el, by = "Group", sort = FALSE, all = TRUE)
    }, indicator_results)
    stopifnot(setequal(res[["Group"]], original_group_ordering))
    res <- res[match(original_group_ordering, res[["Group"]]), , drop = FALSE]

    if (label != "All") {
      res[["Group"]] <- paste0(label, ": ", res[["Group"]])
    }
    res
  }, subset_col_names, subset_labels, SIMPLIFY = FALSE)
  res <- do.call(rbind, subset_results)
  rownames(res) <- NULL
  res
}

prev_zscore_value_column <- function(indicator) {
  paste0("z", indicator[["column"]])
}
prev_zscore_flagged_column <- function(indicator) {
  paste0("f", indicator[["column"]])
}

prev_prevalence_column_name <- function(indicator) {
  if (isTRUE(indicator$with_auxiliary_zscore_column)) {
    paste0(prev_zscore_value_column(indicator), "_aux")
  } else {
    prev_zscore_value_column(indicator)
  }
}

create_zscore_auxiliary_columns <- function(zscores_to_compute, dataframe) {
  oedema <- dataframe[["oedema"]]
  aux_indicators <- Filter(
    function(x) isTRUE(x$with_auxiliary_zscore_column),
    zscores_to_compute
  )
  aux_cols <- vapply(aux_indicators, prev_zscore_value_column, character(1))
  stopifnot(all(aux_cols %in% colnames(dataframe)))
  for (indicator in aux_indicators) {
    col <- prev_zscore_value_column(indicator)
    aux_col <- prev_prevalence_column_name(indicator)
    condition <- if (is.function(indicator$auxiliary_zscore_condition)) {
      indicator$auxiliary_zscore_condition(dataframe)
    } else {
      oedema %in% "y"
    }
    dataframe[[col]][condition] <- NA_real_
    dataframe[[aux_col]] <- as.numeric(
      ifelse(condition, -3.1, dataframe[[col]])
    )
  }
  has_zlen_and_wfl <- all(c("zlen", "zwfl_aux") %in% colnames(dataframe))
  if (has_zlen_and_wfl) {
    make_combined_aux_col <- function(flag_fun) {
      as.integer(
        with(
          dataframe,
          ifelse(
            is.na(zlen) | is.na(zwfl_aux),
            NA_integer_,
            ifelse(zlen < -2 & flag_fun(zwfl_aux), 1L, 0L)
          )
        )
      )
    }
    dataframe[["zlen_zwfl_aux_stunting_wasting"]] <- make_combined_aux_col(
      flag_fun = function(x) x < -2
    )
    dataframe[["zlen_zwfl_aux_stunting_overweight"]] <- make_combined_aux_col(
      flag_fun = function(x) x > 2
    )
  }
  dataframe
}

set_flagged_zscores_to_NA <- function(indicators, dataframe) {
  for (indicator in indicators) {
    zscore_col <- prev_zscore_value_column(indicator)
    flagged_col <- prev_zscore_flagged_column(indicator)
    zscore_val <- dataframe[[zscore_col]]
    flagged_val <- dataframe[[flagged_col]]
    stopifnot(is.integer(flagged_val), is.numeric(zscore_val))
    zscore_val[flagged_val == 1L] <- NA_real_
    dataframe[[zscore_col]] <- zscore_val
  }
  dataframe
}

create_cutoff_columns_for_each_zscore <- function(indicators, dataframe) {
  for (indicator in indicators) {
    for (cutoff in indicator[["cutoffs"]]) {
      value <- cutoff[["fun"]](dataframe[[cutoff[["data_column"]]]])
      new_col_name <- paste0("prev_", indicator$name, cutoff$suffix)
      stopifnot(!(new_col_name %in% colnames(dataframe)))
      dataframe[[new_col_name]] <- value
    }
  }
  dataframe
}

compute_prevalence_sample_size <- function(survey_design, indicator, subset_col_name) {
  indicator_name <- indicator$name
  expr_name <- paste0("I(!is.na(", prev_prevalence_column_name(indicator), "))")
  prev_formula <- as.formula(paste0("~", expr_name))
  subset_formula <- as.formula(paste0("~", subset_col_name))
  prev <- svyby(
    prev_formula,
    subset_formula,
    survey_design$design,
    svytotal,
    drop.empty.groups = FALSE
  )
  unweighted_prev <- svyby(
    prev_formula,
    subset_formula,
    survey_design$design_unweighted,
    svytotal,
    drop.empty.groups = FALSE
  )
  pop_weighted <- prev[[paste0(expr_name, "TRUE")]]
  pop_unweighted <- unweighted_prev[[paste0(expr_name, "TRUE")]]

  # syby returns NA for empty levels. We set the count to 0 for these.
  pop_weighted[is.na(pop_weighted)] <- 0
  pop_unweighted[is.na(pop_unweighted)] <- 0

  stopifnot( # check that subsets come in the right order
    all(prev[[subset_col_name]] == unweighted_prev[[subset_col_name]])
  )
  res <- data.frame(
    Group = as.character(prev[[subset_col_name]]),
    pop = pop_weighted,
    unwpop = pop_unweighted,
    stringsAsFactors = FALSE
  )

  # due to backwards compatibility, columns HA_2_WH_2 and HA_2_WH2 will not
  # have a suffix Z for now
  suffix <- if (indicator_name %in% c("HA_2_WH_2", "HA_2_WH2")) {
    c("_pop", "_unwpop")
  } else {
    c("Z_pop", "Z_unwpop")
  }
  colnames(res) <- c("Group", paste0(indicator_name, suffix))
  # only HA_2_WH_2 has pop and unwpop columns, but not HA_2_WH2
  if (indicator_name == "HA_2_WH2") {
    col_idx_to_drop <- which(colnames(res) %in% c("HA_2_WH2_pop", "HA_2_WH2_unwpop"))
    res <- res[, colnames(res)[-col_idx_to_drop], drop = FALSE]
  }
  res
}

compute_prevalence_cutoff_summaries <- function(survey_design, indicator, subset_col_name) {
  indicator_name <- indicator$name
  lapply(indicator$cutoffs, function(cutoff) {
    prev_col_name <- paste0("prev_", indicator_name, cutoff$suffix)
    compute_prevalence_estimates_for_column(
      survey_design,
      indicator_name = paste0(indicator_name, cutoff$suffix),
      subset_col_name, prev_col_name
    )
  })
}

compute_prevalence_estimates_for_column <- function(survey_design, indicator_name, subset_col_name, prev_col_name) {
  subset_formula <- as.formula(paste0("~", subset_col_name))
  prev_col_formula <- as.formula(paste0("~", prev_col_name))
  all_na <- all(is.na(survey_design$variables[[prev_col_name]]))

  res <- if (all_na) {
    data.frame(
      Group = as.character(unique(survey_design$variables[[subset_col_name]])),
      r = NA_real_,
      se = NA_real_,
      ll = NA_real_,
      ul = NA_real_,
      stringsAsFactors = FALSE
    )
  } else {
    mean_est_prev <- svyby(
      prev_col_formula,
      subset_formula,
      survey_design,
      svymean,
      na.rm = TRUE,
      na.rm.all = TRUE,
      drop.empty.groups = FALSE
    )

    mean_est_ci_prev <- svyby(
      prev_col_formula,
      subset_formula,
      survey_design,
      svyciprop,
      vartype = "ci",
      df = degf(survey_design),
      method = "logit",
      drop.empty.groups = FALSE,
      na.rm.all = TRUE,
      level = 0.95
    )[, 3L:4L]
    data.frame(
      Group = as.character(mean_est_prev[[subset_col_name]]),
      r = mean_est_prev[[prev_col_name]] * 100,
      se = survey::SE(mean_est_prev) * 100,
      ll = mean_est_ci_prev$ci_l * 100,
      ul = mean_est_ci_prev$ci_u * 100,
      stringsAsFactors = FALSE
    )
  }
  value_col_names <- paste0(
    indicator_name, c("_r", "_se", "_ll", "_ul")
  )
  col_names <- c("Group", value_col_names)
  colnames(res) <- col_names
  res
}

compute_prevalence_zscore_summaries <- function(survey_design,
                                                indicator, subset_col_name) {
  indicator_name <- indicator$name
  zscore_col_name <- prev_zscore_value_column(indicator)
  zscore_formula <- as.formula(paste0("~", zscore_col_name))
  subset_formula <- as.formula(paste0("~", subset_col_name))
  all_na <- all(is.na(survey_design$variables[[zscore_col_name]]))
  res <- if (all_na) {
    data.frame(
      Group = as.character(unique(survey_design$variables[[subset_col_name]])),
      r = NA_real_,
      se = NA_real_,
      ll = NA_real_,
      ul = NA_real_,
      stdev = NA_real_,
      stringsAsFactors = FALSE
    )
  } else {
    mean_est_summary <- svyby(
      zscore_formula,
      subset_formula,
      survey_design,
      svymean,
      na.rm = TRUE,
      na.rm.all = TRUE,
      drop.empty.groups = FALSE
    )
    mean_est_ci_summary <- confint(
      mean_est_summary,
      df = degf(survey_design),
      level = 0.95
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
      survey_design,
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

  value_col_names <- paste0(
    indicator_name, c("_r", "_se", "_ll", "_ul", "_stdev")
  )
  colnames(res) <- c("Group", value_col_names)
  res
}
