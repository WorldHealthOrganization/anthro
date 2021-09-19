#' Helper function to compute zscores
#'
#' @param y a numeric vector
#' @param m a numeric vector
#' @param l a numeric vector
#' @param s a numeric vector
#'
#' @export
#' @include z-score-helper.R
anthro_api_compute_zscore <- compute_zscore

#' Helper function to compute the adjusted zscore
#'
#' @param y a numeric vector
#' @param m a numeric vector
#' @param l a numeric vector
#' @param s a numeric vector
#'
#' @export
#' @include z-score-helper.R
anthro_api_compute_zscore_adjusted <- compute_zscore_adjusted

#' Compute prevalence of zscores
#'
#' @param data a data frame containing the underlying data
#' @param zscores_to_compute a list of zscore indicators that should be computed.
#' @param survey_subsets subsets for which the prevalence values should be computed.
#'
#' @note
#' This function is meant to be used by other anthro related packages.
#' It is not advised to use this in you own packages or analysis. If you must
#' use it, prepare for potential breaking changes in the future.
#'
#' @export
#' @include prevalence.R
anthro_api_compute_prevalence <- compute_prevalence_of_zscores

#' Standardize the Oedema input values
#' @param oedema a vector of values
#'
#' @note
#' This function is meant to be used by other anthro related packages.
#' It is not advised to use this in you own packages or analysis. If you must
#' use it, prepare for potential breaking changes in the future.
#'
#' @export
#' @include utils.R
anthro_api_standardize_oedema_var <- standardize_oedema_var

#' Standardize the Sex input values
#' @param sex a vector of values
#'
#' @note
#' This function is meant to be used by other anthro related packages.
#' It is not advised to use this in you own packages or analysis. If you must
#' use it, prepare for potential breaking changes in the future.
#'
#' @export
#' @include utils.R
anthro_api_standardize_sex_var <- standardize_sex_var
