#' Compute the WHO Child Growth Standards
#'
#' @description
#' Provides WHO Child Growth Standards (z-scores) with
#' confidence intervals and standard errors around the
#' prevalence estimates, taking into account complex sample designs.
#' More information on the methods is available online:
#' <\url{https://www.who.int/tools/child-growth-standards}>.
"_PACKAGE"

globalVariables(c(
  "growthstandards_lenanthro",
  "growthstandards_weianthro"
))

# the constant to scale between days and months
ANTHRO_DAYS_OF_MONTH <- 30.4375
