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
#'
#' If not all parameter values have equal length, parameter values will be
#' repeated to match the maximum length of all arguments except
#' \code{is_age_in_month} using \code{rep_len}. This happens without warnings.
#'
#' @inheritParams anthro_zscores
#'
#' @param sw An optional numeric vector containing the sampling weights.
#' If unspecified, the all 1 vector is used, i.e. where all records
#' have equal sampling weights, and un-weighted analysis is performed.
#' Negative values are not allowed.
#'
#' @param cluster An optional integer vector representing clusters
#' @param strata An optional integer vector representing strata
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
#' \dontrun{ # because it takes too long for CRAN checks
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
#' @include anthro.R
#' @include assertions.R
#' @importFrom stats confint
#' @export
anthro_prevalence <- function(sex,
                              age = NA_real_,
                              is_age_in_month = FALSE,
                              weight = NA_real_,
                              lenhei = NA_real_,
                              measure = NA_character_,
                              headc = NA_real_,
                              armc = NA_real_,
                              triskin = NA_real_,
                              subskin = NA_real_,
                              oedema = "n",

                              sw = 1,
                              cluster = 1L,
                              strata = 1L,
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
  assert_numeric(cluster)
  assert_numeric(strata)
  assert_numeric(sw)

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
    length(oedema),
    length(sw),
    length(cluster),
    length(strata),
    length(typeres),
    length(gregion),
    length(wealthq),
    length(mothered),
    length(othergr)
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
  sw <- rep_len(sw, length.out = max_len)
  cluster <- rep_len(cluster, length.out = max_len)
  strata <- rep_len(strata, length.out = max_len)
  typeres <- rep_len(typeres, length.out = max_len)
  gregion <- rep_len(gregion, length.out = max_len)
  wealthq <- rep_len(wealthq, length.out = max_len)
  mothered <- rep_len(mothered, length.out = max_len)
  othergr <- rep_len(othergr, length.out = max_len)

  # zscore data
  zscores <-
    anthro_zscores(
      sex = sex,
      age = age,
      is_age_in_month = is_age_in_month,
      weight = weight,
      lenhei = lenhei,
      measure = measure,
      oedema = oedema
    )

  stopifnot(c("zlen", "zwei", "zwfl", "zbmi") %in% colnames(zscores))
  stopifnot(nrow(zscores) == length(sex))

  age_in_days <- age_to_days(age, is_age_in_month = is_age_in_month)
  age_in_months <- age_in_days / ANTHRO_DAYS_OF_MONTH

  oedema <- standardize_oedema_var(oedema)

  # we compute the age group with full precision
  age_group <- anthro_age_groups(age_in_months)

  # Make z-score as missing if it is flagged
  indicator_names <- c("len", "wei", "bmi", "wfl")
  for (indicator in indicator_names) {
    zscore_col <- paste0("z", indicator)
    flagged_col <- paste0("f", indicator)
    zscore_val <- zscores[[zscore_col]]
    flagged_val <- zscores[[flagged_col]]
    stopifnot(is.integer(flagged_val), is.numeric(zscore_val))
    zscore_val[flagged_val == 1L] <- NA_real_
    zscores[[zscore_col]] <- zscore_val
  }

  # Make Oedema variable to be "n" if age > than 1826 completed days
  # (children w/ oedema count in prevalence even if z-score missing
  # for weight related indicators)
  # this is redundant with the filter that age_in_days <= 1826
  stopifnot(is.character(oedema), all(!is.na(oedema) &
    oedema %in% c("n", "y")))
  oedema_to_n <- !is.na(age_in_days) & age_in_days > 1826
  oedema[oedema_to_n] <- "n"

  # Make weight-related indicators z-scores as missing if it is oedema="y"
  # and create an auxiliar z-score variable with z-score=-3.1 if oedema="y"
  # to be use only to compute prevalences of weight-related indicators
  # - this way, it will count the child in the denominators always and will
  # also count the child in the numerator as well for undernutrition cut-offs.
  # Note, for computing summary statistics for z-scores, the original z-score
  # variables will be used, as information on measured weight is unavailable
  # (or misleading if it is).
  stopifnot(nrow(zscores) == length(oedema))
  for (col in c("zwei", "zwfl", "zbmi")) {
    zscores[[col]][oedema %in% "y"] <- NA_real_
    zscores[[paste0(col, "_aux")]] <-
      as.numeric(ifelse(oedema %in% "y", -3.1, zscores[[col]]))
  }

  if (any(sw < 0, na.rm = TRUE)) {
    stop(
      "Negative sampling weights are not allowed.",
      call. = FALSE
    )
  }

  # For the R Survey package, the id argument (cluster, PSU) is always
  # required, the strata, fpc, weights and probs arguments are optional.
  # If these variables are specified they must not have any missing values.
  # We use cluster and strata in the function to define the design.
  # If one of them is not given, they are replaced by vectors of all 1's
  # (see variable setting in the begining of macro). However, if these
  # variables are provided, we have to use the subset of records where
  # those are not missing.
  # retain only those records where age <= 1826 or is.na(age)
  filter_zscores <- !(is.na(cluster) | is.na(strata))
  filter_zscores <- filter_zscores & (is.na(age_in_days) | age_in_days <= 1826)

  # warn if rows are excluded
  no_excluded <- sum(!filter_zscores, na.rm = TRUE)
  if (no_excluded > 0L) {
    row_label <- if (no_excluded == 1L) "row" else "rows"
    warning(no_excluded, " ", row_label,
      " will be excluded for the prevalence ",
      "computation due to missing cluster, strata or age ",
      "(or age > 1826 days).",
      call. = FALSE
    )
  }

  # set all sw that are NA to 0
  if (anyNA(sw)) {
    na_sw <- is.na(sw)
    sw[na_sw] <- 0
    no_excluded <- sum(na_sw)
    row_label <- if (no_excluded == 1L) "row" else "rows"
    warning(no_excluded, " ", row_label, " had missing sampling weights",
      " and they will be set to 0. ",
      "Doing this excludes them in the prevalence calculation.",
      call. = FALSE
    )
  }

  zscores <- zscores[filter_zscores, , drop = FALSE]
  cluster <- cluster[filter_zscores]
  strata <- strata[filter_zscores]
  typeres <- typeres[filter_zscores]
  gregion <- gregion[filter_zscores]

  wealthq <- as.character(wealthq[filter_zscores])
  valid_wealthq_values <- is.na(wealthq) |
    wealthq %in% as.character(1L:5L) |
    wealthq %in% paste0("Q", 1L:5L)
  if (!all(valid_wealthq_values)) {
    warning("Some entries in wealthq are out of range. Allowed values are: ",
      "NA, 1, 2, 3, 4, 5 or Q1, Q2, Q3, Q4, Q5. ",
      "All other values will be converted to NA",
      call. = FALSE
    )
    wealthq[!valid_wealthq_values] <- NA_character_
  }
  wealth_q_in <- function(x) {
    wealthq %in% c(x, paste0("Q", x))
  }
  wealthq[wealth_q_in("1")] <- "Q1: Poorest"
  wealthq[wealth_q_in("2")] <- "Q2"
  wealthq[wealth_q_in("3")] <- "Q3"
  wealthq[wealth_q_in("4")] <- "Q4"
  wealthq[wealth_q_in("5")] <- "Q5: Richest"
  wealthq <- factor(wealthq, levels = c(
    "Q1: Poorest", "Q2", "Q3",
    "Q4", "Q5: Richest"
  ))

  mothered <- mothered[filter_zscores]
  othergr <- othergr[filter_zscores]
  sw <- sw[filter_zscores]
  age_group <- age_group[filter_zscores]

  sex <- standardize_sex_var(sex)
  sex <- as.character(sex[filter_zscores])
  sex[sex %in% "1"] <- "Male"
  sex[sex %in% "2"] <- "Female"
  sex <- factor(sex, levels = c("Female", "Male"))

  age_sex_interaction <- interaction(age_group, sex)

  # this defines what indicators are being computed
  cutoffs <- c(-3, -2, -1, 1, 2, 3)
  cutoffs_dir <- list(`<`, `<`, `<`, `>`, `>`, `>`)
  cutoffs_suffix <- c("_3", "_2", "_1", "1", "2", "3")
  z_prev <- c("zlen", "zwei_aux", "zwfl_aux", "zbmi_aux")
  z_summ <- c("zlen", "zwei", "zwfl", "zbmi")
  z_lab <- c("HA", "WA", "WH", "BMI")

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
    rep.int("All", length(age_group)), # a strata for total
    age_group,
    sex,
    age_sex_interaction,
    typeres,
    gregion,
    wealthq,
    mothered,
    othergr
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

  # Before we compute all prevalence indicators we temporarily set a
  # survey option and restore it on exit
  opts <- options(survey.lonely.psu = "adjust")
  on.exit(options(opts))

  result_per_stratum <- lapply(seq_len(length(strata_values)), function(i) {
    x <- lapply(seq_len(length(z_lab)), function(j) {
      compute_prevalence(
        cutoffs,
        cutoffs_dir,
        cutoffs_suffix,
        zscore_prev = zscores[[z_prev[[j]]]],
        zscore_summ = zscores[[z_summ[[j]]]],
        zscore_label = z_lab[[j]],
        cluster = cluster,
        strata = strata,
        sampling_weights = sw,
        survey_subsets = strata_values[[i]],
        strata_label = strata_label[[i]]
      )
    })
    results <- do.call(cbind, x)
    results <- results[, unique(colnames(results)), drop = FALSE]

    # To these results we need to add two more indicators
    cbind(
      results,
      compute_stunting_wasting(
        zscores,
        cluster,
        strata,
        sw,
        survey_subsets = strata_values[[i]]
      ),
      compute_stunting_overweight(
        zscores,
        cluster,
        strata,
        sw,
        survey_subsets = strata_values[[i]]
      )
    )
  })

  # now we rbind them together and remove duplicate column names
  results <- do.call(rbind, result_per_stratum)
  results <- results[, unique(colnames(results)), drop = FALSE]

  results
}

compute_prevalence <- function(cutoffs,
                               cutoffs_dir,
                               cutoffs_suffix,
                               zscore_prev,
                               zscore_summ,
                               zscore_label,
                               cluster,
                               strata,
                               sampling_weights,
                               survey_subsets,
                               strata_label) {
  stopifnot(
    length(cutoffs) == length(cutoffs_dir),
    length(cutoffs_dir) == length(cutoffs_suffix)
  )
  stopifnot(is.numeric(cutoffs), is.character(cutoffs_suffix))

  # compute prevalence estimates
  prev_estimate_list <- lapply(seq_len(length(cutoffs)), function(i) {
    cutoffs_suffix <- cutoffs_suffix[[i]]
    cutoff_fun <- cutoffs_dir[[i]]
    cutoff <- cutoffs[[i]]
    var_prev <- cutoff_fun(zscore_prev, cutoff)
    var_prev <- as.integer(ifelse(var_prev, 1L, 0L))

    survey_data <- data.frame(
      cluster = cluster,
      strata = strata,
      sampling_weights = sampling_weights,
      survey_subsets = survey_subsets,
      var_prev = var_prev,
      var_summ = zscore_summ
    )
    df <- compute_prevalence_zscore_summary(survey_data)
    colnames(df) <- c(
      "Group",
      paste0(
        zscore_label,
        c("", "", rep.int(cutoffs_suffix, 4L), rep.int("", 5L)),
        c("", "", rep.int("_", 9L)),
        c(
          "Z_pop",
          "Z_unwpop",
          "r",
          "se",
          "ll",
          "ul",
          "r",
          "se",
          "ll",
          "ul",
          "stdev"
        )
      )
    )

    # split the df into three parts
    # 1. part has global estimates that are equal for all cutoffs
    # 2. part has the estimates for the cutoffs
    # 3. part has also global estimates equal for all cutoffs
    list(
      df[, 1L:3L, drop = FALSE],
      df[, 4L:7L, drop = FALSE],
      df[, 8L:12L, drop = FALSE]
    )
  })

  cutoff_estimates <- lapply(prev_estimate_list, function(x) x[[2L]])
  prev_estimates <- do.call(cbind, cutoff_estimates)

  # remove double stratum column
  prev_estimates <- cbind(
    prev_estimate_list[[1L]][[1L]],
    prev_estimates,
    prev_estimate_list[[1L]][[3L]]
  )

  prev_estimates[["Group"]] <-
    ifelse(
      prev_estimates[["Group"]] != "All",
      paste0(strata_label, ": ", prev_estimates[["Group"]]),
      prev_estimates[["Group"]]
    )

  prev_estimates
}

build_survey_design <- function(survey_data) {
  just_one_cluster <- length(unique(survey_data[["cluster"]])) == 1L
  any_strata_na <- anyNA(survey_data[["strata"]])
  stopifnot(!any_strata_na)
  cluster_formula <- if (just_one_cluster) {
    ~1
  } else {
    ~cluster
  }
  design <-
    survey::svydesign(
      ids = cluster_formula,
      strata = ~strata,
      weights = ~sampling_weights,
      data = survey_data
    )
  design_unweighted <-
    survey::svydesign(
      ids = cluster_formula,
      strata = ~strata,
      weights = ~1,
      data = survey_data
    )
  list(
    design = design,
    design_unweighted = design_unweighted
  )
}

compute_stunting_overweight <- function(zscores, cluster,
                                        strata, sw, survey_subsets) {
  relevant_cols <- c("X_r_prev", "X_se_prev", "X_ll_prev", "X_ul_prev")
  new_colnames <- paste0("HA_2_WH2", c("_r", "_se", "_ll", "_ul"))
  compute_combined_indicator(
    zwfl_aux_flagged = function(x) x > 2,
    relevant_cols, new_colnames,
    zscores, cluster,
    strata, sw, survey_subsets
  )
}

compute_stunting_wasting <- function(zscores, cluster,
                                     strata, sw, survey_subsets) {
  relevant_cols <- c(
    "Z_pop", "Z_unwpop", "X_r_prev", "X_se_prev", "X_ll_prev",
    "X_ul_prev"
  )
  new_colnames <- paste0(
    "HA_2_WH_2",
    c("_pop", "_unwpop", "_r", "_se", "_ll", "_ul")
  )
  compute_combined_indicator(
    zwfl_aux_flagged = function(x) x < -2,
    relevant_cols, new_colnames,
    zscores, cluster,
    strata, sw, survey_subsets
  )
}

compute_combined_indicator <- function(zwfl_aux_flagged,
                                       relevant_columns,
                                       new_column_names,
                                       zscores, cluster,
                                       strata, sw, survey_subsets) {
  var_prev <- as.integer(with(zscores, ifelse(
    is.na(zlen) | is.na(zwfl_aux),
    NA_integer_,
    ifelse(zlen < -2 &
      zwfl_aux_flagged(zwfl_aux), 1L, 0L)
  )))
  survey_data_ha_wh <- data.frame(
    cluster = cluster,
    strata = strata,
    sampling_weights = sw,
    survey_subsets = survey_subsets,
    var_prev = var_prev,
    var_summ = zscores[["zlen"]], # calculations will be ignored
    stringsAsFactors = FALSE
  )
  result_df <- compute_prevalence_zscore_summary(
    survey_data_ha_wh
  )
  result_df <- result_df[, relevant_columns, drop = FALSE]
  colnames(result_df) <- new_column_names
  result_df
}

compute_prevalence_zscore_summary <- function(survey_data) {
  survey_design_list <- build_survey_design(survey_data)
  design <- survey_design_list$design
  design_unweighted <- survey_design_list$design_unweighted

  # Sometimes, survey::svyby et al. warn about glm.fit not converged
  # or "observations with zero weight not used for calculating dispersion"
  # It was decided to suppress these warnings
  suppressWarnings({
    vecn_prev <-
      survey::svyby(
        ~I(!is.na(var_prev)),
        ~survey_subsets,
        design,
        survey::svytotal,
        drop.empty.groups = FALSE
      )
    vecn_unw_prev <-
      survey::svyby(
        ~I(!is.na(var_prev)),
        ~survey_subsets,
        design_unweighted,
        survey::svytotal,
        drop.empty.groups = FALSE
      )
    mean_est_prev <-
      survey::svyby(
        ~var_prev,
        ~survey_subsets,
        design,
        survey::svymean,
        na.rm = TRUE,
        na.rm.all = TRUE,
        drop.empty.groups = FALSE
      )
    mean_est_ci_prev <-
      survey::svyby(
        ~var_prev,
        ~survey_subsets,
        design,
        survey::svyciprop,
        vartype = "ci",
        df = survey::degf(design),
        method = "logit",
        drop.empty.groups = FALSE,
        na.rm.all = TRUE
      )[, 3L:4L]
    mean_est_summ <-
      survey::svyby(
        ~var_summ,
        ~survey_subsets,
        design,
        survey::svymean,
        na.rm = TRUE,
        na.rm.all = TRUE,
        drop.empty.groups = FALSE
      )
    mean_est_ci_summ <-
      confint(mean_est_summ, df = survey::degf(design))

    # the survey package's survey::svyvar fails if
    # there is only one observation with an unexpected error it seems
    # we catch this error here and set all results to NA
    mean_est_sd_summ <- tryCatch({
      survey::svyby(
        ~var_summ,
        ~survey_subsets,
        design,
        survey::svyvar,
        na.rm = TRUE,
        na.rm.all = TRUE,
        drop.empty.groups = FALSE
      )
    }, error = function(er) {
      data.frame(
        dummy = rep.int(NA_real_, nrow(mean_est_ci_summ)),
        result = rep.int(NA_real_, nrow(mean_est_ci_summ))
      )
    })
  })

  df <- data.frame(
    Group = rownames(mean_est_prev),
    Z_pop = vecn_prev$`I(!is.na(var_prev))TRUE`,
    Z_unwpop = vecn_unw_prev$`I(!is.na(var_prev))TRUE`,
    X_r_prev = mean_est_prev$var_prev * 100,
    X_se_prev = mean_est_prev$se * 100,
    X_ll_prev = mean_est_ci_prev$ci_l * 100,
    X_ul_prev = mean_est_ci_prev$ci_u * 100,
    X_r_summ = mean_est_summ$var_summ,
    X_se_summ = survey::SE(mean_est_summ),
    X_ll_summ = mean_est_ci_summ[, 1L, drop = TRUE],
    X_ul_summ = mean_est_ci_summ[, 2L, drop = TRUE],
    X_stdev_summ = sqrt(mean_est_sd_summ[, 2L, drop = TRUE]),
    stringsAsFactors = FALSE
  )

  rownames(df) <- NULL

  df
}
